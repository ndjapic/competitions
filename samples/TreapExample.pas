program TreapExample;
{$MODE DELPHI}
uses SysUtils;

type
  PNode = ^TNode;
  TNode = record
    Value: Int32;
    Priority: Int32;
    Size: Int32;
    Left, Right: PNode;
  end;

var
  Root: PNode = nil;

function GetSize(Node: PNode): Int32;
begin
  if Node = nil then Result := 0 else Result := Node.Size;
end;

procedure UpdateSize(Node: PNode);
begin
  if Node <> nil then
    Node.Size := 1 + GetSize(Node.Left) + GetSize(Node.Right);
end;

// Deljenje stabla na dva dela: manji ili jednaki od Val i veci od Val
procedure Split(Node: PNode; Value: Int32; out L, R: PNode);
begin
  if Node = nil then begin
    L := nil; R := nil;
    exit;
  end;

  if Node.Value < Value then begin
    Split(Node.Right, Value, Node.Right, R);
    L := Node;
  end else begin
    Split(Node.Left, Value, L, Node.Left);
    R := Node;
  end;
  UpdateSize(Node);
end;

// Spajanje dva stabla gde su svi u L manji od svih u R
function Merge(L, R: PNode): PNode;
begin
  if (L = nil) or (R = nil) then begin
    if L <> nil then Result := L else Result := R;
  end else if L.Priority > R.Priority then begin
    L.Right := Merge(L.Right, R);
    Result := L;
  end else begin
    R.Left := Merge(L, R.Left);
    Result := R;
  end;
  UpdateSize(Result);
end;

// 1. ADD: Ubacuje vrednost tako da stablo ostane sortirano
procedure Add(var Node: PNode; Value: Int32);
var
  L, R, NewNode: PNode;
begin
  Split(Node, Value, L, R);
  New(NewNode);
  NewNode.Value := Value;
  NewNode.Priority := Random(MaxInt);
  NewNode.Size := 1;
  NewNode.Left := nil;
  NewNode.Right := nil;
  Node := Merge(Merge(L, NewNode), R);
end;

// 2. GET AT: Vraca vrednost na i-toj poziciji (0-indexed) - O(log n)
function GetAt(Node: PNode; Index: Int32): Int32;
var
  LeftSize: Int32;
begin
  LeftSize := GetSize(Node.Left);
  if Index < LeftSize then
    Result := GetAt(Node.Left, Index)
  else if Index = LeftSize then
    Result := Node.Value
  else
    Result := GetAt(Node.Right, Index - LeftSize - 1);
end;

// 3. DELETE AT: Brisanje elementa na i-toj poziciji - O(log n)
procedure DeleteAt(var Node: PNode; Index: Int32);
var
  L, M, R: PNode;
  {LeftSize: Int32;}

  procedure SplitAt(T: PNode; K: Int32; out L, R: PNode);
  var SZ: Int32;
  begin
    if T = nil then begin L := nil; R := nil; exit; end;
    SZ := GetSize(T.Left);
    if SZ < K then begin
      SplitAt(T.Right, K - SZ - 1, T.Right, R);
      L := T;
    end else begin
      SplitAt(T.Left, K, L, T.Left);
      R := T;
    end;
    UpdateSize(T);
  end;

begin
  SplitAt(Node, Index, L, R);
  SplitAt(R, 1, M, R); // M je element koji brisemo
  Dispose(M);
  Node := Merge(L, R);
end;

function BisectLeft(Node: PNode; Value: Int32): Int32;
begin
  if Node = nil then exit(0);
  if Node.Value >= Value then
    Result := BisectLeft(Node.Left, Value)
  else
    Result := 1 + GetSize(Node.Left) + BisectLeft(Node.Right, Value);
end;

function BisectRight(Node: PNode; Value: Int32): Int32;
begin
  if Node = nil then exit(0);
  if Node.Value > Value then
    Result := BisectRight(Node.Left, Value)
  else
    Result := 1 + GetSize(Node.Left) + BisectRight(Node.Right, Value);
end;

var
  i: integer;
begin
  Randomize;
  
  // Testiranje
  Add(Root, 10);
  Add(Root, 50);
  Add(Root, 20);
  Add(Root, 30); // Stablo ih samo sortira: 10, 20, 30, 50

  writeln('Element na indeksu 2: ', GetAt(Root, 2)); // Ispisuje 30

  DeleteAt(Root, 1); // Brise 20
  
  writeln('Nakon brisanja indeksa 1:');
  for i := 0 to GetSize(Root) - 1 do
    write(GetAt(Root, i), ' '); // Ispisuje 10 30 50
end.
