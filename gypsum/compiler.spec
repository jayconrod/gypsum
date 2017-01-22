# -*- mode: python -*-
import os.path

block_cipher = None

def Files(paths):
    def File(path):
        return os.path.basename(path), path, "DATA"
    return TOC(File(path) for path in paths)

a = Analysis(['driver.py'],
             pathex=['compiler'],
             binaries=None,
             datas=None,
             hiddenimports=[],
             hookspath=[],
             runtime_hooks=[],
             excludes=[],
             win_no_prefer_redirects=False,
             win_private_assemblies=False,
             cipher=block_cipher)
a.datas = Files(os.environ["GYPSUM_COMMON_FILES"].split())
pyz = PYZ(a.pure, a.zipped_data,
             cipher=block_cipher)
exe = EXE(pyz,
          a.scripts,
          a.binaries,
          a.zipfiles,
          a.datas,
          name='compiler',
          debug=False,
          strip=False,
          upx=True,
          console=True )
