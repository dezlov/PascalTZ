{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit pascaltz;

interface

uses
  PascalTZReg, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('PascalTZReg', @PascalTZReg.Register);
end;

initialization
  RegisterPackage('pascaltz', @Register);
end.
