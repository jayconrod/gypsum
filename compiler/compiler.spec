# -*- mode: python -*-
import os.path

def Files(paths):
    def File(path):
        return os.path.basename(path), path, "DATA"
    return TOC(File(path) for path in paths)

a = Analysis(['compiler/compiler'],
             pathex=['compiler'],
             hiddenimports=[],
             hookspath=None,
             runtime_hooks=None)
a.datas = Files(os.environ["GYPSUM_COMMON_FILES"].split())
pyz = PYZ(a.pure)
exe = EXE(pyz,
          a.scripts,
          a.binaries,
          a.zipfiles,
          a.datas,
          name='compiler',
          debug=False,
          strip=None,
          upx=True,
          console=True )
