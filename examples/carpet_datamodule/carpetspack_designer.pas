{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit CarpetsPack_Designer;

interface

uses
  Carpet_Designer, CarpetPropEdits, Carpet_Canvas, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('Carpet_Designer', @Carpet_Designer.Register);
end;

initialization
  RegisterPackage('CarpetsPack_Designer', @Register);
end.
