"""Actual host-protocol regression checks, isolated from any running GUI."""
import sys
sys.dont_write_bytecode = True
import importlib.util
import os
import tempfile
import unittest
import zipfile
from jamovi.server import jamovi_pb2 as pb

spec = importlib.util.spec_from_file_location('inspector', os.path.join(os.path.dirname(__file__), 'inspect-omv.py'))
inspector = importlib.util.module_from_spec(spec)
spec.loader.exec_module(inspector)


class InspectorTest(unittest.TestCase):
    def test_real_host_message_and_missing_plot(self):
        analysis = pb.AnalysisResponse(analysisId=2, name='glmbasic', revision=7, status=pb.ANALYSIS_COMPLETE)
        analysis.options.names.append('out')
        analysis.options.options.add().s = 'y'
        table = analysis.results.group.elements.add(name='glmcat', visible=pb.YES)
        table.table.columns.add(name='var').cells.add().s = '(Intercept)'
        table.table.columns.add(name='means').cells.add().d = 1.25
        table.table.columns.add(name='se').cells.add().o = 1
        image = analysis.results.group.elements.add(name='plot', visible=pb.YES)
        image.image.path = 'resources/plot.png'
        with tempfile.TemporaryDirectory() as directory:
            path = os.path.join(directory, 'owned.omv')
            with zipfile.ZipFile(path, 'w') as archive:
                archive.writestr('02 glmbasic/analysis', analysis.SerializeToString())
            saved = inspector.inspect(path)
            current = saved['analyses'][0]
            self.assertEqual(current['options'], {'out': 'y'})
            self.assertEqual(current['revision'], 7)
            self.assertEqual(current['results']['children'][0]['columns'][1]['values'], [1.25])
            self.assertEqual(current['results']['children'][0]['columns'][2]['values'], [None])
            self.assertFalse(current['results']['children'][1]['resource_exists'])
            self.assertEqual(saved['source'], 'saved_gui_document')
            with zipfile.ZipFile(path, 'a') as archive:
                archive.writestr('02 glmbasic/resources/plot.png', b'controlled-image-fixture')
            self.assertTrue(inspector.inspect(path)['analyses'][0]['results']['children'][1]['resource_exists'])


if __name__ == '__main__':
    unittest.main()
