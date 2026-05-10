program _E;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
// #simple #avltree #min #max
uses
	Math;
const
	MAX_NODES = 200 * 1000;
type
	TNode = record
		Key: Int32;
		Count, Height, Left, Right: Int32;
	end;
var
	Tree: array[0 .. MAX_NODES] of TNode;
	Root: Int32 = 0;
	NodeCount: Int32 = 0;

function NewNode(K: Int32): Int32; inline;
begin
	Inc(NodeCount);
	Tree[NodeCount].Key := K;
	Tree[NodeCount].Count := 1;
	Tree[NodeCount].Height := 1;
	Tree[NodeCount].Left := 0;
	Tree[NodeCount].Right := 0;
	Result := NodeCount;
end;

function GetHeight(N: Int32): Int32; inline;
begin
	if N = 0 then
		Result := 0
	else
		Result := Tree[N].Height;
end;

procedure UpdateHeight(N: Int32); inline;
begin
	Tree[N].Height := 1 + Max(GetHeight(Tree[N].Left), GetHeight(Tree[N].Right));
end;

function GetBalance(N: Int32): Int32; inline;
begin
	if N = 0 then
		Result := 0
	else
		Result := GetHeight(Tree[N].Left) - GetHeight(Tree[N].Right);
end;

function RightRotate(Y: Int32): Int32;
var X, T2: Int32;
begin
	X := Tree[Y].Left;
	T2 := Tree[X].Right;
	Tree[X].Right := Y;
	Tree[Y].Left := T2;
	UpdateHeight(Y);
	UpdateHeight(X);
	Result := X;
end;

function LeftRotate(X: Int32): Int32;
var Y, T2: Int32;
begin
	Y := Tree[X].Right;
	T2 := Tree[Y].Left;
	Tree[Y].Left := X;
	Tree[X].Right := T2;
	UpdateHeight(X);
	UpdateHeight(Y);
	Result := Y;
end;

function BalanceNode(N: Int32): Int32;
var Balance: Int32;
begin
	UpdateHeight(N);
	Balance := GetBalance(N);
	if Balance > 1 then begin
		if GetBalance(Tree[N].Left) < 0 then
			Tree[N].Left := LeftRotate(Tree[N].Left);
		Result := RightRotate(N);
	end else if Balance < -1 then begin
		if GetBalance(Tree[N].Right) > 0 then
			Tree[N].Right := RightRotate(Tree[N].Right);
		Result := LeftRotate(N);
	end else Result := N;
end;

function Insert(N: Int32; K: Int32): Int32;
begin
	if N = 0 then
		Result := NewNode(K)
	else if K = Tree[N].Key then begin
		Inc(Tree[N].Count);
		Result := N;
	end else begin
		if K < Tree[N].Key then
			Tree[N].Left := Insert(Tree[N].Left, K)
		else
			Tree[N].Right := Insert(Tree[N].Right, K);
		Result := BalanceNode(N);
	end;
end;

function FindMinNode(N: Int32): Int32; inline;
begin
	Result := N;
	if N <> 0 then
		while Tree[Result].Left <> 0 do
			Result := Tree[Result].Left;
end;

function FindMaxNode(N: Int32): Int32; inline;
begin
	Result := N;
	if N > 0 then
		while Tree[Result].Right <> 0 do
			Result := Tree[Result].Right;
end;

function Delete(N: Int32; K: Int32; C: Int32): Int32;
var Temp: Int32;
begin
	Result := N;
	if N > 0 then begin
		if K < Tree[N].Key then
			Tree[N].Left := Delete(Tree[N].Left, K, C)
		else if K > Tree[N].Key then
			Tree[N].Right := Delete(Tree[N].Right, K, C)
		else if Tree[N].Count > C then
			Dec(Tree[N].Count, C)
		else if Tree[N].Right = 0 then
			Result := Tree[N].Left
		else if Tree[N].Left = 0 then
			Result := Tree[N].Right
		else begin
			Temp := FindMinNode(Tree[N].Right);
			Tree[N].Key := Tree[Temp].Key;
			Tree[N].Count := Tree[Temp].Count;
			// Бришемо заменски чвор из десног подстабла
			Tree[N].Right := Delete(Tree[N].Right, Tree[Temp].Key, Tree[Temp].Count);
		end;
		if Result > 0 then Result := BalanceNode(Result);
	end;
end;

var
	q, i, tp, x, c: Int32;
	MinN, MaxN: Int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	if not SeekEof then Read(q);
	for i := 1 to q do begin
		Read(tp);
		case tp of

			1: begin
				Read(x);
				Root := Insert(Root, x);
			end;

			2: begin
				Read(x, c);
				Root := Delete(Root, x, c);
			end;

			3: begin
				MinN := FindMinNode(Root);
				MaxN := FindMaxNode(Root);
				if Root <> 0 then
					Writeln(Tree[MaxN].Key - Tree[MinN].Key)
				else
					Writeln(0);
			end;

		end;
	end;
end.
