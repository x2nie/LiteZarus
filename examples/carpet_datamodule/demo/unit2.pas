unit Unit2;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, BufDataset, db, Carpets;

type

  { TDataRoom1 }

  TDataRoom1 = class(TDataRoom)
    BufDataset1: TBufDataset;
    Carpet1: TCarpet;
    Carpet2: TCarpet;
    Carpet3: TCarpet;
    Carpet4: TCarpet;
    Carpet5: TCarpet;
    Carpet6: TCarpet;
    Carpet7: TCarpet;
    Carpet8: TCarpet;
    DataSource1: TDataSource;
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

