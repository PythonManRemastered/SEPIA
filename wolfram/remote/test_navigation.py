import unittest
from unittest.mock import Mock
import pandas as pd
from sepia_pds_bridge import Bridge
from test_bridge import Result

class NavigationTests(unittest.TestCase):
    def setUp(self):
        self.b = Bridge()
        self.c = Mock()
        self.b.connection = self.c
        self.t = self.c.CASTable.return_value
        self.t.shape = (2, 2)
        self.t.columns = ['ID', 'VALUE']
        self.c.table.loadTable.return_value = Result()
        self.listing()
    def listing(self, name='folder', perm='drwxr-xr-x', severity=0):
        self.c.table.fileInfo.return_value = Result(severity=severity, FileInfo=pd.DataFrame([
            {'Name': name, 'Size': 20, 'Permission': perm}]))
    def browse(self):
        return self.b.dispatch('browse', ['arbitrary_new_uid'])
    def test_arbitrary_uid_and_folder(self):
        root=self.browse()
        self.assertTrue(root['OK'])
        self.listing('adsl.sas7bdat', '-rw-r--r--')
        nested=self.b.dispatch('select',[root['ListingToken'],1])
        self.assertEqual(nested['Entries'][0]['RelativePath'],'folder/adsl.sas7bdat')
        self.c.table.fileInfo.assert_called_with(caslib='arbitrary_new_uid',allFiles=True,path='folder')
        loaded=self.b.dispatch('select',[nested['ListingToken'],1])
        self.assertEqual(loaded['ContributionID'],'arbitrary_new_uid')
        metadata=self.b.dispatch('inspect',[loaded['Token']])
        self.assertEqual(metadata['ColumnNames'],['ID','VALUE'])
        self.t.fetch.assert_not_called()
    def test_explicit_mapping(self):
        out=self.b.dispatch('browse',['UID','','KnownCaslib'])
        self.assertEqual(out['Caslib'],'KnownCaslib')
        self.assertEqual(out['ContributionID'],'UID')
    def test_unsupported_file(self):
        self.listing('data.zip','-rw-r--r--')
        out=self.browse()
        self.assertEqual(self.b.dispatch('select',[out['ListingToken'],1])['Code'],'UnsupportedEntry')
        self.c.table.loadTable.assert_not_called()
    def test_invalid_selection(self):
        out=self.browse()
        for value in [0,2,-1,True,1.5]:
            self.assertEqual(self.b.dispatch('select',[out['ListingToken'],value])['Code'],'InvalidSelection')
    def test_stale_listing(self):
        out=self.browse()
        self.b.close()
        self.b.connection=self.c
        self.assertEqual(self.b.dispatch('select',[out['ListingToken'],1])['Code'],'UnknownListing')
    def test_failed_listing(self):
        self.listing(severity=2)
        self.assertEqual(self.browse()['Code'],'CASActionFailed')
        self.assertEqual(self.b.listings,{})
    def test_warning(self):
        self.listing(severity=1)
        self.assertEqual(self.browse()['Severity'],1)
    def test_qualified_path(self):
        self.listing('folder/adsl.sas7bdat','-rw-r--r--')
        out=self.b.dispatch('browse',['UID','folder'])
        self.assertEqual(out['Entries'][0]['RelativePath'],'folder/adsl.sas7bdat')
    def test_ambiguous_and_unsafe_path(self):
        for name in ['other/file.sas7bdat','../file.sas7bdat','/file.sas7bdat']:
            self.listing(name,'-rw-r--r--')
            self.assertFalse(self.b.dispatch('browse',['UID','folder'])['OK'])
    def test_metadata_mismatch(self):
        loaded=self.b.dispatch('load',['UID','adsl.sas7bdat'])
        self.t.columns=['ID','ID']
        self.assertEqual(self.b.dispatch('inspect',[loaded['Token']])['Code'],'ColumnMismatch')
        self.t.fetch.assert_not_called()
    def test_unknown_kind_not_loaded(self):
        self.listing('adsl.sas7bdat','')
        out=self.browse()
        self.assertFalse(out['Entries'][0]['SupportedImport'])

if __name__=='__main__':
    unittest.main()
