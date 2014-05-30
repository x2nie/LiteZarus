REM Adjust the path to yours path
SET FPC_BIN_PATH=C:\laz4android\fpc\2.7.1

REM Create fpc.cfg
%FPC_BIN_PATH%\bin\i386-win32\fpcmkcfg.exe -d basepath=%FPC_BIN_PATH% -o %FPC_BIN_PATH%\bin\i386-win32\fpc.cfg

REM Set FPC Path
Set Path=%FPC_BIN_PATH%\bin\i386-win32

REM Make lazarus
make clean all bigide

REM Strip
strip.exe lazarus.exe
strip.exe lazbuild.exe
strip.exe startlazarus.exe