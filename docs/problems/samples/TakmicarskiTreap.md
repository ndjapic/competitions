# Problem: TakmicarskiTreap.pas

```pascal
program TakmicarskiTreap;

{$MODE DELPHI}
{$H+}

uses SysUtils;

type
  TTacka = record
    x, y: Integer;
  end;

  PNode = ^TNode;
  TNode = record
    Key: TTacka;
    Priority: Integer;
    Size: Integer;    // Број чворова у поддрвету
    Left, Right: PNode;
  end;

function Compare(const L, R: TTacka): Integer;
begin
  if L.y <> R.y then Result := R.y - L.y 
  else Result := L.x - R.x;
end;

function GetSize(N: PNode): Integer;
begin
  if N = nil then Result := 0 else Result := N.Size;
end;

procedure UpdateSize(N: PNode);
begin
  if N <> nil then
    N.Size := 1 + GetSize(N.Left) + GetSize(N.Right);
end;

procedure Split(Root: PNode; Key: TTacka; out L, R: PNode);
begin
  if Root = nil then
  begin
    L := nil; R := nil;
  end
  else if Compare(Root.Key, Key) < 0 then
  begin
    Split(Root.Right, Key, Root.Right, R);
    L := Root;
    UpdateSize(L);
  end
  else
  begin
    Split(Root.Left, Key, L, Root.Left);
    R := Root;
    UpdateSize(R);
  end;
end;

function Merge(L, R: PNode): PNode;
begin
  if L = nil then Result := R
  else if R = nil then Result := L
  else if L.Priority > R.Priority then
  begin
    L.Right := Merge(L.Right, R);
    Result := L;
  end
  else
  begin
    R.Left := Merge(L, R.Left);
    Result := R;
  end;
  UpdateSize(Result);
end;

function NewNode(K: TTacka): PNode;
begin
  New(Result);
  Result.Key := K;
  Result.Priority := Random(MaxInt);
  Result.Size := 1;
  Result.Left := nil;
  Result.Right := nil;
end;

var
  Root: PNode = nil;

procedure Insert(K: TTacka);
var L, R: PNode;
begin
  Split(Root, K, L, R);
  // Дупликати: Ова имплементација подржава дупликате (исте тачке).
  // Ако их не желиш, у Insert функцији додај проверу пре NewNode.
  Root := Merge(Merge(L, NewNode(K)), R);
end;

procedure Delete(K: TTacka);
var L, Mid, R: PNode;
begin
  Split(Root, K, L, R);
  Split(R, K, Mid, R);
  if Mid <> nil then
  begin
    // Бришемо само корен Mid дрвета (један примерак тачке)
    // Ако желиш да обришеш СВЕ дупликате, рекурзивно обриши цео Mid
    Root := Merge(L, Merge(Merge(Mid.Left, Mid.Right), R));
    Dispose(Mid);
  end
  else
    Root := Merge(L, R);
end;

function CountRangeY(YMin, YMax: Integer): Integer;
var
  L, Mid, R: PNode;
  TMin, TMax: TTacka;
begin
  TMax.y := YMax; TMax.x := -MaxInt;
  TMin.y := YMin; TMin.x := MaxInt;

  Split(Root, TMax, L, R);
  Split(R, TMin, Mid, R);
  
  Result := GetSize(Mid); // Овде добијамо број у O(log N)

  Root := Merge(L, Merge(Mid, R)); // Враћамо дрво у оригинално стање
end;

begin
  Randomize;
  // Тестирање
  Insert(Default(TTacka)); // Подеси x, y пре позива
  WriteLn('Broj tacaka u opsegu: ', CountRangeY(50, 100));
end.

```
