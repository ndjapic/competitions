program _E;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
// #simple #treap #min #max
uses
	Math;
type
	PNode = ^TNode;
	TNode = record
		Key: Int32;
		Priority, Size, Count: Int32;
		Left, Right: PNode;
	end;
var
	q, i, x, c, tp: Int32;
	Root: PNode = nil;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

function GetSize(N: PNode): Int32; inline;
begin
	if N = nil then
		Result := 0
	else
		Result := N^.Size;
end;

procedure Update(N: PNode); inline;
begin
	if N <> nil then
		N^.Size := GetSize(N^.Left) + GetSize(N^.Right) + N^.Count;
end;

function NewNode(K: Int32): PNode;
begin
	New(Result);
	Result^.Key := K;
	Result^.Priority := Random(MaxInt);
	Result^.Count := 1;
	Result^.Size := 1;
	Result^.Left := nil;
	Result^.Right := nil;
end;

procedure Split(N: PNode; K: Int32; out L, R: PNode);
begin
	if N = nil then begin
		L := nil;
		R := nil;
	end else if N^.Key < K then begin
		Split(N^.Right, K, N^.Right, R);
		L := N;
	end else begin
		Split(N^.Left, K, L, N^.Left);
		R := N;
	end;
	Update(N);
end;

procedure Merge(var N: PNode; L, R: PNode);
begin
	if R = nil then
		N := L
	else if L = nil then
		N := R
	else if L^.Priority > R^.Priority then begin
		Merge(L^.Right, L^.Right, R);
		N := L;
	end else begin
		Merge(R^.Left, L, R^.Left);
		N := R;
	end;
	Update(N);
end;

procedure Add(var N: PNode; K: Int32);
var L, M, R: PNode;
begin
	Split(N, K, L, R);
	Split(R, K + 1, M, R);

	if M <> nil then begin
		Inc(M^.Count);
		Update(M);
	end else
		M := NewNode(K);

	Merge(L, L, M);
	Merge(N, L, R);
end;

procedure Remove(var N: PNode; K, C: Int32);
var
	L, M, R: PNode;
begin
	Split(N, K, L, R);
	Split(R, K + 1, M, R);
	if M <> nil then begin

		if M^.Count > C then begin
			Dec(M^.Count, C);
			Update(M);
			Merge(L, L, M);
		end else
			Dispose(M);

	end;
	Merge(N, L, R);
end;

function GetMin(N: PNode): Int32;
begin
	while N^.Left <> nil do
		N := N^.Left;
	Result := N^.Key;
end;

function GetMax(N: PNode): Int32;
begin
	while N^.Right <> nil do
		N := N^.Right;
	Result := N^.Key;
end;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);
	Randomize;

	Readln(q);
	for i := 1 to q do begin
		Read(tp);
		case tp of

			1: begin
				Read(x);
				Add(Root, x);
			end;

			2: begin
				Read(x, c);
				Remove(Root, x, c);
			end;

			3: if Root <> nil then
				Writeln(GetMax(Root) - GetMin(Root));

		end;
		Readln;
	end;
end.
