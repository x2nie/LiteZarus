unit Unit2;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Carpets;

type

  { TDataRoom1 }

  TDataRoom1 = class(TDataRoom)
    Carpet1: TCarpet;
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  DataRoom1: TDataRoom1;

implementation

{$R *.lfm}

end.

