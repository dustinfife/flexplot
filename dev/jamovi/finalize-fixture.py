"""Populate saved option messages using jamovi's installed serializer.

Standalone jmvcore analyses compute real results but do not initialize their
host-provided option protobuf. Without this step a file can display saved
results yet lose its model specification when the analysis is rerun.
"""
import json
import os
import sys
import zipfile
from jamovi.server import jamovi_pb2 as messages
from jamovi.server.options import Options, read_value_from_pb

archive, manifest_path = sys.argv[1:]
with open(manifest_path, encoding="utf-8") as stream:
    manifest = json.load(stream)
replacements = {}
with zipfile.ZipFile(archive) as source:
    for index, configuration in enumerate(manifest["configurations"], 1):
        name = configuration["name"]
        definitions = manifest["definitions"][name]
        options = Options.create(definitions)
        types = {option["name"]: option["type"] for option in definitions}
        for key, value in configuration["options"].items():
            if types.get(key) == "Variables" and isinstance(value, str):
                value = [value]
            if types.get(key) == "Terms":
                value = [[term] if isinstance(term, str) else term for term in value]
            options.set_value(key, value)
        entry = f"{index*2:02d} {name}/analysis"
        response = messages.AnalysisResponse()
        response.ParseFromString(source.read(entry))
        response.options.CopyFrom(options.as_pb())
        assert options.get_value("out") == configuration["options"]["out"]
        assert options.get_value("modelTerms")
        replacements[entry] = response.SerializeToString()
    temporary = archive + ".updated"
    with zipfile.ZipFile(temporary, "w", zipfile.ZIP_DEFLATED) as target:
        for entry in source.infolist():
            target.writestr(entry, replacements.get(entry.filename, source.read(entry.filename)))
os.replace(temporary, archive)
print("Verified saved options for all four analyses.")
