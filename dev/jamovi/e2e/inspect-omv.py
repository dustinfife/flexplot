"""Read saved host protobufs without connecting to or changing a running instance."""
import hashlib
import io
import json
import math
import sys
import zipfile
from jamovi.server import jamovi_pb2 as pb
from jamovi.server.options import read_values_from_pb


def cell_value(cell):
    kind = cell.WhichOneof('cellType')
    value = getattr(cell, kind) if kind and kind != 'o' else None
    return None if isinstance(value, float) and not math.isfinite(value) else value


def element(node, archive, prefix):
    kind = node.WhichOneof('type')
    result = dict(name=node.name, title=node.title, status=pb.AnalysisStatus.Name(node.status),
                  visible=pb.Visible.Name(node.visible), stale=node.stale,
                  error=node.error.message, type=kind)
    if kind in ('group', 'array'):
        result['children'] = [element(child, archive, prefix) for child in getattr(node, kind).elements]
    elif kind == 'table':
        result['columns'] = [dict(name=c.name, title=c.title, visible=pb.Visible.Name(c.visible),
                                  values=[cell_value(cell) for cell in c.cells]) for c in node.table.columns]
        result['notes'] = {note.key: note.note for note in node.table.notes}
    elif kind == 'html':
        result['content'] = node.html.content
    elif kind == 'image':
        path = prefix + '/' + node.image.path
        # Resource paths may already include the analysis prefix.
        if path not in archive.namelist():
            path = node.image.path
        result['resource'] = path
        data = archive.read(path) if path in archive.namelist() else b''
        result['resource_exists'] = bool(data)
        result['resource_sha256'] = hashlib.sha256(data).hexdigest() if data else None
    return result


def inspect(path):
    with open(path, "rb") as stream:
        raw = stream.read()
    with zipfile.ZipFile(io.BytesIO(raw)) as archive:
        analyses = []
        for name in archive.namelist():
            if not name.endswith('/analysis'):
                continue
            response = pb.AnalysisResponse()
            response.ParseFromString(archive.read(name))
            analyses.append(dict(id=response.analysisId, name=response.name, title=response.title,
                namespace=response.ns, revision=response.revision, status=pb.AnalysisStatus.Name(response.status),
                error=response.error.message, options=read_values_from_pb(response.options),
                results=element(response.results, archive, name.rsplit('/', 1)[0])))
        return dict(source='saved_gui_document', document=path,
                    dataset_schema=json.loads(archive.read('metadata.json')).get('dataSet') if 'metadata.json' in archive.namelist() else None,
                    sha256=hashlib.sha256(raw).hexdigest(), analyses=analyses)


if __name__ == '__main__':
    json.dump(inspect(sys.argv[1]), sys.stdout, allow_nan=False)
    print()
