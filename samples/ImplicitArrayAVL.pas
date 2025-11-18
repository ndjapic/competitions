{$MODE DELPHI}
program ImplicitArrayAVL;

{---------------------------------------------------------------
 Generic implicit array with AVL tree + lazy segment tree features
 - Works under {$MODE DELPHI} using Delphi-style generics
 - Usage:
       var ia: TImplicitArray<Int32>;
       ia := TImplicitArray<Int32>.Create;
 - Supports:
       InsertAt(i, x)          O(log n)
       DeleteAt(i)             O(log n)
       GetAt(i)
       SetAt(i, x)
       RangeAdd(l, r, d)       add d on interval
       RangeMax(l, r)          max on interval
 - Node stores: height, cnt, max, lazy_add
---------------------------------------------------------------}

uses
  SysUtils;

type
  { Generic implicit AVL array }
  TImplicitArray<T> = class
  private
    type
      PNode = ^TNode;
      TNode = record
        val: T;
        left, right: PNode;
        height: Integer;
        cnt: Integer;
        mx: T;
        lazy: T;
      end;

    var
      root: PNode;

    function NewNode(const v: T): PNode;
    procedure PushUp(p: PNode);
    procedure PushDown(p: PNode);
    function Height(p: PNode): Integer; inline;
    function CountOf(p: PNode): Integer; inline;
    function GetMx(p: PNode): T; inline;

    function RotateLeft(x: PNode): PNode;
    function RotateRight(y: PNode): PNode;
    function Balance(p: PNode): PNode;

    function GetAtNode(p: PNode; idx: Integer): T;
    function SetAtNode(p: PNode; idx: Integer; const v: T): PNode;
    function InsertAtNode(p: PNode; idx: Integer; const v: T): PNode;
    function DeleteAtNode(p: PNode; idx: Integer): PNode;

    function RangeMaxNode(p: PNode; l, r: Integer): T;
    procedure RangeAddNode(p: PNode; l, r: Integer; const d: T);
    procedure FreeNode(p: PNode);

  public
    constructor Create;
    destructor Destroy; override;

    function Count: Integer;

    function GetAt(idx: Integer): T;
    procedure SetAt(idx: Integer; const v: T);
    procedure InsertAt(idx: Integer; const v: T);
    procedure DeleteAt(idx: Integer);

    procedure RangeAdd(l, r: Integer; const d: T);
    function RangeMax(l, r: Integer): T;
  end;


{---------------------------------------}
{ Implementation }

function TImplicitArray<T>.Height(p: PNode): Integer;
begin
  if p = nil then Exit(0); Result := p^.height;
end;

function TImplicitArray<T>.CountOf(p: PNode): Integer;
begin
  if p = nil then Exit(0); Result := p^.cnt;
end;

function TImplicitArray<T>.GetMx(p: PNode): T;
begin
  if p = nil then Exit(Low(T));
  Result := p^.mx;
end;

function TImplicitArray<T>.NewNode(const v: T): PNode;
begin
  New(Result);
  Result^.val := v;
  Result^.left := nil;
  Result^.right := nil;
  Result^.height := 1;
  Result^.cnt := 1;
  Result^.mx := v;
  Result^.lazy := 0;
end;

procedure TImplicitArray<T>.PushUp(p: PNode);
begin
  p^.cnt := 1 + CountOf(p^.left) + CountOf(p^.right);
  p^.mx := p^.val;
  if (p^.left <> nil) and (p^.left^.mx > p^.mx) then p^.mx := p^.left^.mx;
  if (p^.right <> nil) and (p^.right^.mx > p^.mx) then p^.mx := p^.right^.mx;
  p^.height := 1 + Max(Height(p^.left), Height(p^.right));
end;

procedure TImplicitArray<T>.PushDown(p: PNode);
begin
  if (p = nil) or (p^.lazy = 0) then Exit;
  if p^.left <> nil then
  begin
    Inc(p^.left^.val, p^.lazy);
    Inc(p^.left^.mx, p^.lazy);
    Inc(p^.left^.lazy, p^.lazy);
  end;
  if p^.right <> nil then
  begin
    Inc(p^.right^.val, p^.lazy);
    Inc(p^.right^.mx, p^.lazy);
    Inc(p^.right^.lazy, p^.lazy);
  end;
  p^.lazy := 0;
end;

function TImplicitArray<T>.RotateLeft(x: PNode): PNode;
var y, T2: PNode;
begin
  y := x^.right;
  T2 := y^.left;
  y^.left := x;
  x^.right := T2;
  PushUp(x);
  PushUp(y);
  Result := y;
end;

function TImplicitArray<T>.RotateRight(y: PNode): PNode;
var x, T2: PNode;
begin
  x := y^.left;
  T2 := x^.right;
  x^.right := y;
  y^.left := T2;
  PushUp(y);
  PushUp(x);
  Result := x;
end;

function TImplicitArray<T>.Balance(p: PNode): PNode;
var bf: Integer;
begin
  if p = nil then Exit(nil);
  PushUp(p);
  bf := Height(p^.left) - Height(p^.right);

  if bf > 1 then
  begin
    if Height(p^.left^.right) > Height(p^.left^.left) then
      p^.left := RotateLeft(p^.left);
    Exit(RotateRight(p));
  end;
  if bf < -1 then
  begin
    if Height(p^.right^.left) > Height(p^.right^.right) then
      p^.right := RotateRight(p^.right);
    Exit(RotateLeft(p));
  end;
  Result := p;
end;

function TImplicitArray<T>.GetAtNode(p: PNode; idx: Integer): T;
var L: Integer;
begin
  PushDown(p);
  L := CountOf(p^.left);
  if idx < L then Exit(GetAtNode(p^.left, idx));
  if idx = L then Exit(p^.val);
  Exit(GetAtNode(p^.right, idx - L - 1));
end;

function TImplicitArray<T>.SetAtNode(p: PNode; idx: Integer; const v: T): PNode;
var L: Integer;
begin
  PushDown(p);
  L := CountOf(p^.left);
  if idx < L then p^.left := SetAtNode(p^.left, idx, v)
  else if idx = L then p^.val := v
  else p^.right := SetAtNode(p^.right, idx - L - 1, v);
  PushUp(p);
  Result := Balance(p);
end;

function TImplicitArray<T>.InsertAtNode(p: PNode; idx: Integer; const v: T): PNode;
var L: Integer;
begin
  if p = nil then Exit(NewNode(v));
  PushDown(p);
  L := CountOf(p^.left);
  if idx <= L then p^.left := InsertAtNode(p^.left, idx, v)
  else p^.right := InsertAtNode(p^.right, idx - L - 1, v);
  PushUp(p);
  Result := Balance(p);
end;

function TImplicitArray<T>.DeleteAtNode(p: PNode; idx: Integer): PNode;
var L: Integer; q: PNode;
begin
  PushDown(p);
  L := CountOf(p^.left);
  if idx < L then p^.left := DeleteAtNode(p^.left, idx)
  else if idx > L then p^.right := DeleteAtNode(p^.right, idx - L - 1)
  else begin
    if (p^.left = nil) or (p^.right = nil) then
    begin
      if p^.left <> nil then q := p^.left else q := p^.right;
      Dispose(p);
      Exit(q);
    end
    else begin
      q := p^.right;
      PushDown(q);
      while q^.left <> nil do
      begin q := q^.left; PushDown(q); end;
      p^.val := q^.val;
      p^.right := DeleteAtNode(p^.right, 0);
    end;
  end;
  PushUp(p);
  Result := Balance(p);
end;

function TImplicitArray<T>.RangeMaxNode(p: PNode; l, r: Integer): T;
var L, idx: Integer; res, t: T;
begin
  if (p = nil) or (l > r) then Exit(Low(T));
  PushDown(p);
  L := CountOf(p^.left);
  idx := L;

  res := Low(T);

  if l < idx then
  begin
    t := RangeMaxNode(p^.left, l, Min(r, idx-1));
    if t > res then res := t;
  end;
  if (l <= idx) and (idx <= r) then
  begin
    if p^.val > res then res := p^.val;
  end;
  if r > idx then
  begin
    t := RangeMaxNode(p^.right, Max(0, l-idx-1), r-idx-1);
    if t > res then res := t;
  end;
  Result := res;
end;

procedure TImplicitArray<T>.RangeAddNode(p: PNode; l, r: Integer; const d: T);
var L, idx: Integer;
begin
  if (p = nil) or (l > r) then Exit;
  if (l = 0) and (r = CountOf(p)-1) then
  begin
    Inc(p^.val, d);
    Inc(p^.mx, d);
    Inc(p^.lazy, d);
    Exit;
  end;
  PushDown(p);
  L := CountOf(p^.left);
  idx := L;

  if l < idx then
    RangeAddNode(p^.left, l, Min(r, idx-1), d);
  if (l <= idx) and (idx <= r) then
    Inc(p^.val, d);
  if r > idx then
    RangeAddNode(p^.right, Max(0, l-idx-1), r-idx-1, d);

  PushUp(p);
end;

procedure TImplicitArray<T>.FreeNode(p: PNode);
begin
  if p = nil then Exit;
  FreeNode(p^.left);
  FreeNode(p^.right);
  Dispose(p);
end;

constructor TImplicitArray<T>.Create;
begin
  root := nil;
end;

destructor TImplicitArray<T>.Destroy;
begin
  FreeNode(root);
  inherited;
end;

function TImplicitArray<T>.Count: Integer;
begin
  Result := CountOf(root);
end;

function TImplicitArray<T>.GetAt(idx: Integer): T;
begin
  Result := GetAtNode(root, idx);
end;

procedure TImplicitArray<T>.SetAt(idx: Integer; const v: T);
begin
  root := SetAtNode(root, idx, v);
end;

procedure TImplicitArray<T>.InsertAt(idx: Integer; const v: T);
begin
  root := InsertAtNode(root, idx, v);
end;

procedure TImplicitArray<T>.DeleteAt(idx: Integer);
begin
  root := DeleteAtNode(root, idx);
end;

procedure TImplicitArray<T>.RangeAdd(l, r: Integer; const d: T);
begin
  RangeAddNode(root, l, r, d);
end;

function TImplicitArray<T>.RangeMax(l, r: Integer): T;
begin
  Result := RangeMaxNode(root, l, r);
end;

begin
  { Demo – delete or modify for contest usage }
  var ia: TImplicitArray<Int32>;
  ia := TImplicitArray<Int32>.Create;
  ia.InsertAt(0, 5);
  ia.InsertAt(1, 7);
  ia.InsertAt(2, 3);
  ia.RangeAdd(0,2,2); { +2 to all }
  Writeln(ia.RangeMax(0,2)); { prints 9 }
  ia.Free;
end.
