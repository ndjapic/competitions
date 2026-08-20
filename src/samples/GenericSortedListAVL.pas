program GenericSortedListAVL;

{$MODE DELPHI}

uses
	SysUtils, Generics.Defaults, Math;

type
	TSortedList<T> = class
	private
		type
			PNode = ^TNode;
			TNode = record
				Value: T;
				Height: Integer;
				SubtreeSize: Integer; { Кључно за O(log N) приступ преко индекса }
				Left, Right: PNode;
			end;
	private
		FRoot: PNode;
		FComparer: IComparer<T>;

		function GetHeight(Node: PNode): Integer; inline;
		function GetSize(Node: PNode): Integer; inline;
		procedure UpdateNode(Node: PNode); inline;
		function GetBalance(Node: PNode): Integer; inline;

		function RotateRight(Y: PNode): PNode;
		function RotateLeft(X: PNode): PNode;
		function BalanceNode(Node: PNode): PNode;

		function InsertNode(Node: PNode; const X: T; out InsertedNode: PNode): PNode;
		function DeleteNode(Node: PNode; const X: T; out Deleted: Boolean): PNode;
		function FindMin(Node: PNode): PNode;

		function FindBisectLeft(Node: PNode; const X: T; CurrentOffset: Integer): Integer;
		function FindBisectRight(Node: PNode; const X: T; CurrentOffset: Integer): Integer;
		function FindByRank(Node: PNode; Rank: Integer): T;

		procedure FreeTree(Node: PNode);
		function GetCount: Integer; inline;
		function GetItem(Index: Integer): T; inline;
	public
		constructor Create(AComparer: IComparer<T>);
		destructor Destroy; override;

		procedure Add(const X: T);					{ O(log N) }
		procedure Discard(const X: T);				{ O(log N) }
		function BisectLeft(const X: T): Integer;	{ O(log N) }
		function BisectRight(const X: T): Integer;	{ O(log N) }

		property Count: Integer read GetCount;
		property Items[Index: Integer]: T read GetItem; default;
	end;

{ --- Имплементација помоћних AVL метода --- }

constructor TSortedList<T>.Create(AComparer: IComparer<T>);
begin
	inherited Create;
	FRoot := nil;
	FComparer := AComparer;
end;

destructor TSortedList<T>.Destroy;
begin
	FreeTree(FRoot);
	inherited Destroy;
end;

procedure TSortedList<T>.FreeTree(Node: PNode);
begin
	if Node <> nil then begin
		FreeTree(Node.Left);
		FreeTree(Node.Right);
		Dispose(Node);
	end;
end;

function TSortedList<T>.GetHeight(Node: PNode): Integer;
begin
	if Node = nil then Result := 0 else Result := Node.Height;
end;

function TSortedList<T>.GetSize(Node: PNode): Integer;
begin
	if Node = nil then Result := 0 else Result := Node.SubtreeSize;
end;

procedure TSortedList<T>.UpdateNode(Node: PNode);
var
	LH, RH: Integer;
begin
	LH := GetHeight(Node.Left);
	RH := GetHeight(Node.Right);
	Node.Height := Max(LH, RH) + 1;
	Node.SubtreeSize := GetSize(Node.Left) + GetSize(Node.Right) + 1;
end;

function TSortedList<T>.GetBalance(Node: PNode): Integer;
begin
	if Node = nil then
		Result := 0
	else
		Result := GetHeight(Node.Left) - GetHeight(Node.Right);
end;

function TSortedList<T>.RotateRight(Y: PNode): PNode;
var
	X, T2: PNode;
begin
	X := Y.Left;
	T2 := X.Right;
	X.Right := Y;
	Y.Left := T2;
	UpdateNode(Y);
	UpdateNode(X);
	Result := X;
end;

function TSortedList<T>.RotateLeft(X: PNode): PNode;
var
	Y, T2: PNode;
begin
	Y := X.Right;
	T2 := Y.Left;
	Y.Left := X;
	X.Right := T2;
	UpdateNode(X);
	UpdateNode(Y);
	Result := Y;
end;

function TSortedList<T>.BalanceNode(Node: PNode): PNode;
var
	Balance: Integer;
begin
	UpdateNode(Node);
	Balance := GetBalance(Node);

	if Balance > 1 then begin
		if GetBalance(Node.Left) < 0 then
			Node.Left := RotateLeft(Node.Left);
		Exit(RotateRight(Node));
	end;

	if Balance < -1 then begin
		if GetBalance(Node.Right) > 0 then
			Node.Right := RotateRight(Node.Right);
		Exit(RotateLeft(Node));
	end;

	Result := Node;
end;

{ --- Имплементација главних метода у O(log N) --- }

function TSortedList<T>.InsertNode(Node: PNode; const X: T; out InsertedNode: PNode): PNode;
begin
	if Node = nil then begin
		New(InsertedNode);
		InsertedNode.Value := X;
		InsertedNode.Height := 1;
		InsertedNode.SubtreeSize := 1;
		InsertedNode.Left := nil;
		InsertedNode.Right := nil;
		Exit(InsertedNode);
	end;

	{ За стабилно подржавање дупликата, веће ИЛИ ЈЕДНАКЕ гурамо десно }
	if FComparer.Compare(X, Node.Value) < 0 then
		Node.Left := InsertNode(Node.Left, X, InsertedNode)
	else
		Node.Right := InsertNode(Node.Right, X, InsertedNode);

	Result := BalanceNode(Node);
end;

procedure TSortedList<T>.Add(const X: T);
var
	Temp: PNode;
begin
	FRoot := InsertNode(FRoot, X, Temp);
end;

function TSortedList<T>.FindMin(Node: PNode): PNode;
begin
	while Node.Left <> nil do Node := Node.Left;
	Result := Node;
end;

function TSortedList<T>.DeleteNode(Node: PNode; const X: T; out Deleted: Boolean): PNode;
var
	Cmp: Integer;
	Temp: PNode;
begin
	if Node <> nil then begin

		Cmp := FComparer.Compare(X, Node.Value);
		if Cmp < 0 then
			Node.Left := DeleteNode(Node.Left, X, Deleted)
		else if Cmp > 0 then
			Node.Right := DeleteNode(Node.Right, X, Deleted)
		else begin
			{ Пронађен је елемент }
			Deleted := True;
			if (Node.Left = nil) or (Node.Right = nil) then begin

				if Node.Left <> nil then Temp := Node.Left else Temp := Node.Right;
				if Temp = nil then begin
					Temp := Node;
					Node := nil;
				end else
					Node^ := Temp^;
				Dispose(Temp);

			end else begin

				Temp := FindMin(Node.Right);
				Node.Value := Temp.Value;
				Node.Right := DeleteNode(Node.Right, Temp.Value, Deleted);

			end;
		end;

	end;

	if Node = nil then
		Result := nil
	else
		Result := BalanceNode(Node);
end;

procedure TSortedList<T>.Discard(const X: T);
var
	Deleted: Boolean;
begin
	Deleted := False;
	FRoot := DeleteNode(FRoot, X, Deleted);
end;

{ --- Претраге по рангу и Бисекције у O(log N) --- }

function TSortedList<T>.FindBisectLeft(Node: PNode; const X: T; CurrentOffset: Integer): Integer;
begin
	if Node = nil then
		Result := CurrentOffset
	else if FComparer.Compare(Node.Value, X) >= 0 then
		Result := FindBisectLeft(Node.Left, X, CurrentOffset)
	else
		Result := FindBisectLeft(Node.Right, X, CurrentOffset + GetSize(Node.Left) + 1);
end;

function TSortedList<T>.BisectLeft(const X: T): Integer;
begin
	Result := FindBisectLeft(FRoot, X, 0);
end;

function TSortedList<T>.FindBisectRight(Node: PNode; const X: T; CurrentOffset: Integer): Integer;
begin
	if Node = nil then
		Result := CurrentOffset
	else if FComparer.Compare(Node.Value, X) > 0 then
		Result := FindBisectRight(Node.Left, X, CurrentOffset)
	else
		Result := FindBisectRight(Node.Right, X, CurrentOffset + GetSize(Node.Left) + 1);
end;

function TSortedList<T>.BisectRight(const X: T): Integer;
begin
	Result := FindBisectRight(FRoot, X, 0);
end;

function TSortedList<T>.FindByRank(Node: PNode; Rank: Integer): T;
var
	LeftSize: Integer;
begin
	LeftSize := GetSize(Node.Left);
	if Rank < LeftSize then
		Result := FindByRank(Node.Left, Rank)
	else if Rank > LeftSize then
		Result := FindByRank(Node.Right, Rank - LeftSize - 1)
	else
		Result := Node.Value;
end;

function TSortedList<T>.GetItem(Index: Integer): T;
begin
	if (Index < 0) or (Index >= Count) then
		raise ERangeError.Create('Индекс је ван опсега!');
	Result := FindByRank(FRoot, Index);
end;

function TSortedList<T>.GetCount: Integer;
begin
	Result := GetSize(FRoot);
end;


{ --- ТЕСТ ПРОГРАМ --- }

function slCompare(constref Left, Right: Integer): Integer;
begin
	Result := CompareValue(Left, Right);
end;

var
	List: TSortedList<Integer>;
	i: Integer;
begin
	List := TSortedList<Integer>.Create(TComparer<Integer>.Construct(slCompare));
	try
		List.Add(10);
		List.Add(5);
		List.Add(20);
		List.Add(10); { Дупликат }
		List.Add(15);

		Write('Листа из AVL стабла: ');
		for i := 0 to List.Count - 1 do
			Write(List[i], ' '); { GetAt је избачен, користи се индекс директно у O(log N) }
		WriteLn;

		WriteLn('BisectLeft(10): ', List.BisectLeft(10));	// Очекивано: 1
		WriteLn('BisectRight(10): ', List.BisectRight(10));	// Очекивано: 3

		List.Discard(10);
		Write('Након уклањања једне 10-ке: ');
		for i := 0 to List.Count - 1 do Write(List[i], ' ');
		WriteLn;

	finally
		List.Free;
	end;
	ReadLn;
end.
