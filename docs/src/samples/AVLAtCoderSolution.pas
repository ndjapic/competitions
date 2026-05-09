program AtCoderSolution;
{$mode delphi}

type
	PNode = ^TNode;
	TNode = record
		Key, Height: Integer;
		Left, Right: PNode;
	end;

function NewNode(K: Integer): PNode;
begin
	New(Result);
	Result^.Key := K; Result^.Height := 1;
	Result^.Left := nil; Result^.Right := nil;
end;

function GetHeight(N: PNode): Integer; inline;
begin if N = nil then Result := 0 else Result := N^.Height; end;

function Max(A, B: Integer): Integer; inline;
begin if A > B then Result := A else Result := B; end;

function GetBalance(N: PNode): Integer; inline;
begin if N = nil then Result := 0 else Result := GetHeight(N^.Left) - GetHeight(N^.Right); end;

function RightRotate(Y: PNode): PNode;
var X, T2: PNode;
begin
	X := Y^.Left; T2 := X^.Right;
	X^.Right := Y; Y^.Left := T2;
	Y^.Height := Max(GetHeight(Y^.Left), GetHeight(Y^.Right)) + 1;
	X^.Height := Max(GetHeight(X^.Left), GetHeight(X^.Right)) + 1;
	Result := X;
end;

function LeftRotate(X: PNode): PNode;
var Y, T2: PNode;
begin
	Y := X^.Right; T2 := Y^.Left;
	Y^.Left := X; X^.Right := T2;
	X^.Height := Max(GetHeight(X^.Left), GetHeight(X^.Right)) + 1;
	Y^.Height := Max(GetHeight(Y^.Left), GetHeight(Y^.Right)) + 1;
	Result := Y;
end;

function Insert(Node: PNode; Key: Integer): PNode;
var Balance: Integer;
begin
	if Node = nil then Exit(NewNode(Key));
	if Key < Node^.Key then Node^.Left := Insert(Node^.Left, Key)
	else if Key > Node^.Key then Node^.Right := Insert(Node^.Right, Key)
	else Exit(Node);

	Node^.Height := 1 + Max(GetHeight(Node^.Left), GetHeight(Node^.Right));
	Balance := GetBalance(Node);

	if (Balance > 1) and (Key < Node^.Left^.Key) then Exit(RightRotate(Node));
	if (Balance < -1) and (Key > Node^.Right^.Key) then Exit(LeftRotate(Node));
	if (Balance > 1) and (Key > Node^.Left^.Key) then begin Node^.Left := LeftRotate(Node^.Left); Exit(RightRotate(Node)); end;
	if (Balance < -1) and (Key < Node^.Right^.Key) then begin Node^.Right := RightRotate(Node^.Right); Exit(LeftRotate(Node)); end;
	Result := Node;
end;

// Ово су еквиваленти за FindLowest и FindHighest
function FindMin(Node: PNode): PNode;
begin
	Result := Node;
	while Result^.Left <> nil do Result := Result^.Left;
end;

function FindMax(Node: PNode): PNode;
begin
	Result := Node;
	while Result^.Right <> nil do Result := Result^.Right;
end;

var
	Root: PNode = nil;
	MinN, MaxN: PNode;
begin
	// Пример коришћења унутар такмичарског кода:
	Root := Insert(Root, 50);
	Root := Insert(Root, 30);
	Root := Insert(Root, 70);
	
	MinN := FindMin(Root);
	MaxN := FindMax(Root);
	
	Writeln('Разлика: ', MaxN^.Key - MinN^.Key);
end.
