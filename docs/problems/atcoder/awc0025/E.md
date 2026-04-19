# Problem: E.pas

```pascal
program _E;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	Math;

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
			end;

		var
			root: PNode;

		function NewNode(const v: T): PNode;
		procedure PushUp(p: PNode);
		function Height(p: PNode): Int32; inline;
		function CountOf(p: PNode): Int32; inline;

		function RotateLeft(x: PNode): PNode;
		function RotateRight(y: PNode): PNode;
		function Balance(p: PNode): PNode;

		function GetAtNode(p: PNode; idx: Int32): T;
		function SetAtNode(p: PNode; idx: Int32; const v: T): PNode;
		function InsertAtNode(p: PNode; idx: Int32; const v: T): PNode;
		function DeleteAtNode(p: PNode; idx: Int32): PNode;
		procedure FreeNode(p: PNode);

	public
		constructor Create;
		destructor Destroy; override;

		function Count: Int32;

		function GetAt(idx: Int32): T;
		procedure SetAt(idx: Int32; const v: T);
		procedure InsertAt(idx: Int32; const v: T);
		procedure DeleteAt(idx: Int32);
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

function TImplicitArray<T>.NewNode(const v: T): PNode;
begin
	New(Result);
	Result^.val := v;
	Result^.left := nil;
	Result^.right := nil;
	Result^.height := 1;
	Result^.cnt := 1;
end;

procedure TImplicitArray<T>.PushUp(p: PNode);
begin
	p^.cnt := 1 + CountOf(p^.left) + CountOf(p^.right);
	p^.height := 1 + Max(Height(p^.left), Height(p^.right));
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
			p := RotateRight(p);
		end else if bf < -1 then begin
			if Height(p^.right^.left) > Height(p^.right^.right) then
				p^.right := RotateRight(p^.right);
			p := RotateLeft(p);
		end;
	end;
	Result := p;
end;

function TImplicitArray<T>.GetAtNode(p: PNode; idx: Int32): T;
var
	L: Int32;
begin
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
			while q^.left <> nil do
			begin q := q^.left; end;
			p^.val := q^.val;
			p^.right := DeleteAtNode(p^.right, 0);

		end;
	end;

	PushUp(p);
	Result := Balance(p);
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

var
	n, q, i, j, di, t: int32;
	d: TImplicitArray<Int32>;

begin
	readln(n, q);

	d := TImplicitArray<Int32>.Create;
	for i := 0 to n-1 do begin
		read(di);
		d.InsertAt(i, di);
	end;
	readln;

	for j := 1 to q do begin
		readln(t);
		if t <= n then begin
			dec(t);
			di := d.GetAt(t) - 1;
			if di = 0 then begin
				d.DeleteAt(t);
				dec(n);
			end else
				d.SetAt(t, di);
		end;
		writeln(n);
	end;

	d.Free;
end.

```
