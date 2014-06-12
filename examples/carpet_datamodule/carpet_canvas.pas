unit Carpet_Canvas;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Carpets, Graphics;

type

  { TLCLCarpetCanvas }

  TLCLCarpetCanvas = class(TCarpetCanvas)
  private
    FLCLcanvas: TCanvas;
  protected
    property LCLCanvas : TCanvas read FLCLcanvas write FLCLcanvas;
  public
    procedure FillRect(const X1,Y1,X2,Y2: Integer; const AColor: Cardinal); override;
    procedure Frame3D(var ARect: TRect; TopColor, BottomColor: Cardinal;
                      const FrameWidth: integer); override;
    procedure Rectangle(X1,Y1,X2,Y2: Integer; AColor: Cardinal); override;
    procedure StretchDraw(const DestRect: TRect; SrcGraphic: TObject); override;
    procedure TextOut(X,Y: Integer; const Text: String); override;
    procedure TextRect(ARect: TRect; const Text: string; Alignment: TAlignment); override;

  end;



implementation

{ TLCLCarpetCanvas }

procedure TLCLCarpetCanvas.FillRect(const X1, Y1, X2, Y2: Integer;
  const AColor: Cardinal);
begin
  if not assigned(LCLCanvas) then exit;
  LCLCanvas.Brush.Color:=AColor;
  LCLCanvas.Brush.Style:=bsSolid;
  LCLCanvas.FillRect(x1,y1,x2,y2);
end;

procedure TLCLCarpetCanvas.Frame3D(var ARect: TRect; TopColor,
  BottomColor: Cardinal; const FrameWidth: integer);
begin
  if not assigned(LCLCanvas) then exit;
  LCLCanvas.Frame3D(ARect,TopColor,BottomColor,FrameWidth);
end;

procedure TLCLCarpetCanvas.Rectangle(X1, Y1, X2, Y2: Integer; AColor: Cardinal);
begin
  if not assigned(LCLCanvas) then exit;
  LCLCanvas.Pen.Color:=AColor;
  LCLCanvas.Rectangle(x1,y1,x2,y2);
end;

procedure TLCLCarpetCanvas.StretchDraw(const DestRect: TRect;
  SrcGraphic: TObject);
begin
  if not assigned(LCLCanvas) then exit;
  LCLCanvas.StretchDraw(DestRect, TGraphic(SrcGraphic));
end;

procedure TLCLCarpetCanvas.TextOut(X, Y: Integer; const Text: String);
begin
  if not assigned(LCLCanvas) then exit;
  LCLCanvas.Font.Style:= [fsBold];
  LCLCanvas.Brush.Style:=bsClear;
  LCLCanvas.TextOut(X,Y,Text);
end;

procedure TLCLCarpetCanvas.TextRect(ARect: TRect; const Text: string;
  Alignment: TAlignment);
var style : TTextStyle;
begin
  style.Alignment:=Alignment;
  style.Wordbreak := True;
  style.EndEllipsis:=True;
  style.Clipping:=True;
  LCLCanvas.TextRect(ARect, ARect.Left, ARect.Top, text, style);
end;


end.

