{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit Carpet_GraphicalPack;

interface

uses
  Carpet_Images, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('Carpet_Images', @Carpet_Images.Register);
end;

initialization
  RegisterPackage('Carpet_GraphicalPack', @Register);
end.
