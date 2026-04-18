# Задатак: G_Fine_Triplets.pas

```pascal
program G_Fine_Triplets;
{$MODE DELPHI}
const
    nn = 1000 * 1000;
    hh = 65535;

{interface}

type
    TBitWise = class
    private
        FItems: array of qword;
        FSize: int32;
    public
        constructor Create;
        destructor Destroy; override;
        procedure SetSize(n: int32);
        function GetBit(i: int32): Boolean;
        procedure SetBit(i: int32; b: boolean);
        procedure ShiftLeft(d: int32);
        procedure BitAnd(a, b: TBitWise; var c: TBitWise);
        function Hamming(): int8;
        property Bits[Bit: LongInt]: Boolean
            read GetBit write SetBit; default;
        property Size: LongInt read FSize write SetSize;
    end;

{implementation}

constructor TBitWise.Create;
begin
    SetLength(Fitems, 1);
    FSize := 0;
end;

destructor TBitWise.Destroy;
begin
    SetLength(Fitems, 1);
    FSize := 0;
end;

procedure TBitWise.SetSize(n: int32);
begin
    FSize := n;
    if Length(FItems) * 64 < n then SetLength(FItems, n div 32 + 1);
end;

function TBitWise.GetBit(i: int32): boolean;
begin
    Result := odd(FItems[i div 64] shr (i mod 64));
end;

procedure TBitWise.SetBit(i: int32; b: boolean);
var
    j: int32;
    p2: QWord;
begin
    j := i mod 64;
    i := i div 64;
    p2 := int32(1) shl j;
    if b then
        FItems[i] := FItems[i] or p2
    else
        FItems[i] := FItems[i] and (High(QWord) - p2);
end;

procedure TBitWise.ShiftLeft(d: int32);
(* TODO: have to optimize for large values of d. *)
var
    i: int32;
begin
    for i := FSize downto d do SetBit(i, GetBit(i-d));
end;

procedure TBitWise.BitAnd(a, b: TBitWise; var c: TBitWise);
var
    n, i: int32;
begin
    n := a.Size;
    c.Size := n;
    for i := 0 to (n-1) div 64 do
        c.FItems[i] := a.FItems[i] and b.FItems[i];

    i := (n-1) div 64;
    n := n mod 64;
    if n > 0 then
        c.FItems[i] := c.FItems[i] and ((int32(1) shl n) -1);
end;

var
    n, i, s: int32;
    seen, a, c: TBitWise;
    ham: array [0 .. hh] of int8;

function TBitWise.Hamming(): int8;
var
    i: int32;
    x: int64;
begin
    Result := 0;
    for i := 0 to (n-1) div 64 do begin
        x := FItems[i];
        inc(Result, ham[x and hh]);
        inc(Result, ham[(x shr 16) and hh]);
        inc(Result, ham[(x shr 32) and hh]);
        inc(Result, ham[(x shr 48) and hh]);
    end;
end;

begin
    ham[0] := 0;
    for n := 0 to hh div 2 do begin
        ham[2*n] := ham[n];
        ham[2*n+1] := ham[n] + 1;
    end;

    readln(n);

    seen := TBitWise.Create;
    seen.Size := nn;
    for s := 0 to nn-1 do seen[s-1] := false;

    for i := 1 to n do begin
        read(s);
        seen[s-1] := true;
    end;
    readln;

    for i := 0 to n-1 do begin
        write(a.Get(i));
        if i < n-1 then write(' ');
    end;
    writeln;
end.

```
