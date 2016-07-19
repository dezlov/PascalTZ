unit PascalTZReg;

{$mode objfpc}{$H+}

interface

procedure Register;

implementation

uses
  Classes, LResources, uPascalTZ;

procedure Register;
begin
  RegisterComponents('Misc', [TPascalTZ]);
end;

end.
