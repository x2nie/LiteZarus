{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit CarpetsPack;

interface

uses
  Carpet_Designer, Carpets, CarpetPropEdits, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('Carpet_Designer', @Carpet_Designer.Register);
end;

initialization
  RegisterPackage('CarpetsPack', @Register);
end.
