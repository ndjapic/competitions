# Задатак: ImplicitArrayAVL.pas

```pascal
{$MODE DELPHI}{$INLINE ON}
program ImplicitArrayAVL;

(*---------------------------------------------------------------
 Generic implicit array with AVL tree + lazy segment tree features
 - Works under {$MODE DELPHI} using Delphi-style generics
 - Usage:
		var ia: TImplicitArray<Int32>;
		ia := TImplicitArray<Int32>.Create;
 - Supports:
		InsertAt(i, x)				O(log n)
		DeleteAt(i)					O(log n)
		GetAt(i)
		SetAt(i, x)
		RangeUpdate(l, r, d)		add d on interval
		RangeQuery(l, r)			max on interval
 - Node stores: height, cnt, max, lazy_add
---------------------------------------------------------------*)

uses
	SysUtils, Math;

type
	{ Generic implicit AVL array }
	TImplicitArray<T> = class
	private
		type
			PNode = ^TNode;
			TNode = record
				val: T;
				left, right: PNode;
				height: Int32;
				cnt: Int32;
				acc: T;
				lazy: T;
			end;

		var
			root: PNode;

		function NewNode(const v: T): PNode;
		procedure PushUp(p: PNode);
		procedure PushDown(p: PNode);
		function Height(p: PNode): Int32; inline;
		function CountOf(p: PNode): Int32; inline;
		function GetAcc(p: PNode): T; inline;

		function RotateLeft(x: PNode): PNode;
		function RotateRight(y: PNode): PNode;
		function Balance(p: PNode): PNode;

		function GetAtNode(p: PNode; idx: Int32): T;
		function SetAtNode(p: PNode; idx: Int32; const v: T): PNode;
		function InsertAtNode(p: PNode; idx: Int32; const v: T): PNode;
		function DeleteAtNode(p: PNode; idx: Int32): PNode;

		function RangeQueryNode(p: PNode; l, r: Int32): T;
		procedure RangeUpdateNode(p: PNode; l, r: Int32; const d: T);
		procedure FreeNode(p: PNode);

	public
		constructor Create;
		destructor Destroy; override;

		function Count: Int32;

		function GetAt(idx: Int32): T;
		procedure SetAt(idx: Int32; const v: T);
		procedure InsertAt(idx: Int32; const v: T);
		procedure DeleteAt(idx: Int32);

		procedure RangeUpdate(l, r: Int32; const d: T);
		function RangeQuery(l, r: Int32): T;
	end;

{---------------------------------------}
{ Implementation }

function TImplicitArray<T>.Height(p: PNode): Int32;
begin
	if p = nil then Result := 0 else Result := p^.height;
end;

function TImplicitArray<T>.CountOf(p: PNode): Int32;
begin
	if p = nil then Result := 0 else Result := p^.cnt;
end;

{ Replace Low(T) with neutral value of query operation. }
function TImplicitArray<T>.GetAcc(p: PNode): T;
begin
	if p = nil then Result := Low(T) else Result := p^.acc;
end;

function TImplicitArray<T>.NewNode(const v: T): PNode;
begin
	New(Result);
	Result^.val := v;
	Result^.left := nil;
	Result^.right := nil;
	Result^.height := 1;
	Result^.cnt := 1;
	Result^.acc := v;
	Result^.lazy := 0; { Replace 0 with neutral value of update operation. }
end;

procedure TImplicitArray<T>.PushUp(p: PNode);
begin
	p^.cnt := 1 + CountOf(p^.left) + CountOf(p^.right);
	p^.acc := p^.val;
	{ Replace two Max after then with query operation. }
	if p^.left <> nil then p^.acc := Max(p^.acc, p^.left^.acc);
	if p^.right <> nil then p^.acc := Max(p^.acc, p^.right^.acc);
	p^.height := 1 + Max(Height(p^.left), Height(p^.right));
end;

{ Replace Inc with update operation. }
procedure TImplicitArray<T>.PushDown(p: PNode);
begin
	if (p <> nil) and (p^.lazy <> 0) then begin
		if p^.left <> nil then begin
			Inc(p^.left^.val, p^.lazy);
			Inc(p^.left^.acc, p^.lazy);
			Inc(p^.left^.lazy, p^.lazy);
		end;
		if p^.right <> nil then begin
			Inc(p^.right^.val, p^.lazy);
			Inc(p^.right^.acc, p^.lazy);
			Inc(p^.right^.lazy, p^.lazy);
		end;
		p^.lazy := 0; { Replace 0 with neutral value of update operation. }
	end;
end;

function TImplicitArray<T>.RotateLeft(x: PNode): PNode;
var
	y: PNode;
begin
	y := x^.right;
	x^.right := y^.left;
	y^.left := x;
	PushUp(x);
	PushUp(y);
	Result := y;
end;

function TImplicitArray<T>.RotateRight(y: PNode): PNode;
var
	x: PNode;
begin
	x := y^.left;
	y^.left := x^.right;
	x^.right := y;
	PushUp(y);
	PushUp(x);
	Result := x;
end;

function TImplicitArray<T>.Balance(p: PNode): PNode;
var
	bf: Int32;
begin
	if p <> nil then begin
		PushUp(p);
		bf := Height(p^.left) - Height(p^.right);

		if bf > 1 then begin
			if Height(p^.left^.right) > Height(p^.left^.left) then
				p^.left := RotateLeft(p^.left);
			{Exit(RotateRight(p));}
			p := RotateRight(p);
		end else if bf < -1 then begin
			if Height(p^.right^.left) > Height(p^.right^.right) then
				p^.right := RotateRight(p^.right);
			{Exit(RotateLeft(p));}
			p := RotateLeft(p);
		end;
	end;
	Result := p;
end;

function TImplicitArray<T>.GetAtNode(p: PNode; idx: Int32): T;
var
	L: Int32;
begin
	PushDown(p);
	L := CountOf(p^.left);

	if idx < L then
		Result := GetAtNode(p^.left, idx)
	else if idx = L then
		Result := p^.val
	else
		Result := GetAtNode(p^.right, idx - L - 1);
end;

function TImplicitArray<T>.SetAtNode(p: PNode; idx: Int32; const v: T): PNode;
var
	L: Int32;
begin
	PushDown(p);
	L := CountOf(p^.left);

	if idx < L then
		p^.left := SetAtNode(p^.left, idx, v)
	else if idx = L then
		p^.val := v
	else
		p^.right := SetAtNode(p^.right, idx - L - 1, v);

	PushUp(p);
	Result := Balance(p);
end;

function TImplicitArray<T>.InsertAtNode(p: PNode; idx: Int32; const v: T): PNode;
var
	L: Int32;
begin
	if p = nil then
		Result := NewNode(v)
	else begin
		PushDown(p);
		L := CountOf(p^.left);

		if idx <= L then
			p^.left := InsertAtNode(p^.left, idx, v)
		else
			p^.right := InsertAtNode(p^.right, idx - L - 1, v);

		PushUp(p);
		Result := Balance(p);
	end;
end;

function TImplicitArray<T>.DeleteAtNode(p: PNode; idx: Int32): PNode;
var
	L: Int32;
	q: PNode;
begin
	PushDown(p);
	L := CountOf(p^.left);

	if idx < L then
		p^.left := DeleteAtNode(p^.left, idx)
	else if idx > L then
		p^.right := DeleteAtNode(p^.right, idx - L - 1)
	else begin
		if (p^.left = nil) or (p^.right = nil) then begin

			if p^.left <> nil then q := p^.left else q := p^.right;
			Dispose(p);
			Exit(q);

		end else begin

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

procedure TImplicitArray<T>.RangeUpdateNode(p: PNode; l, r: Int32; const d: T);
var
	idx: Int32;
begin
	if (p <> nil) and (l <= r) then begin
		if (l <= 0) and (r >= CountOf(p)-1) then begin
			{ Replace Inc with update operation. }
			Inc(p^.val, d);
			Inc(p^.acc, d);
			Inc(p^.lazy, d);
		end else begin
			PushDown(p);
			idx := CountOf(p^.left);

			if l < idx then RangeUpdateNode(p^.left, l, Min(r, idx-1), d);
			{ Replace Inc with update operation. }
			if (l <= idx) and (idx <= r) then Inc(p^.val, d);
			if r > idx then RangeUpdateNode(p^.right, Max(0, l-idx-1), r-idx-1, d);

			PushUp(p);
		end;
	end;
end;

function TImplicitArray<T>.RangeQueryNode(p: PNode; l, r: Int32): T;
var
	idx: Int32;
	res: T;
begin
	{ Replace Low(T) with neutral value of query operation. }
	res := Low(T);
	if (p <> nil) and (l <= r) then begin
		PushDown(p);
		idx := CountOf(p^.left);

		{ Replace three Max with query operation. }
		if l < idx then
			res := Max(res, RangeQueryNode(p^.left, l, Min(r, idx-1)));
		if (l <= idx) and (idx <= r) then
			res := Max(res, p^.val);
		if r > idx then
			res := Max(res, RangeQueryNode(p^.right, Max(0, l-idx-1), r-idx-1));
	end;
	Result := res;
end;

procedure TImplicitArray<T>.FreeNode(p: PNode);
begin
	if p <> nil then begin
		FreeNode(p^.left);
		FreeNode(p^.right);
		Dispose(p);
	end;
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

function TImplicitArray<T>.Count: Int32;
begin
	Result := CountOf(root);
end;

function TImplicitArray<T>.GetAt(idx: Int32): T;
begin
	Result := GetAtNode(root, idx);
end;

procedure TImplicitArray<T>.SetAt(idx: Int32; const v: T);
begin
	root := SetAtNode(root, idx, v);
end;

procedure TImplicitArray<T>.InsertAt(idx: Int32; const v: T);
begin
	root := InsertAtNode(root, idx, v);
end;

procedure TImplicitArray<T>.DeleteAt(idx: Int32);
begin
	root := DeleteAtNode(root, idx);
end;

procedure TImplicitArray<T>.RangeUpdate(l, r: Int32; const d: T);
begin
	RangeUpdateNode(root, l, r, d);
end;

function TImplicitArray<T>.RangeQuery(l, r: Int32): T;
begin
	Result := RangeQueryNode(root, l, r);
end;

var
	n, i, h: int32;
	ia: TImplicitArray<Int32>;

begin
	{ Demo – delete or modify for contest usage }
	n := 100 * 1000;

	ia := TImplicitArray<Int32>.Create;
	for i := 0 to n-1 do ia.InsertAt(i, i);
	h := n div 2;
	ia.RangeUpdate(h, n-1, -h); { -h to second half }
	Writeln(ia.RangeQuery(100, n-101)); { prints 49999 }
	ia.Free;

end.

```
