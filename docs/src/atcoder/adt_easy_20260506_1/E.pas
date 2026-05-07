program _E;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	AVL_Tree, Generics.Defaults;
type
	TSortedDictionary<TKey, TValue> = class
	public
		type
			PNodeData = ^TNodeData;
			TNodeData = record
				Key: TKey;
				Value: TValue;
			end;
	private
		FTree: TAVLTree;
		class function CompareNodes(D1, D2: Pointer): Integer; static;
	public
		constructor Create;
		procedure Clear;
		destructor Destroy; override;
		procedure Add(const K: TKey; const V: TValue);
		function TryGetValue(const K: TKey; out V: TValue): Boolean;
		function FindLowest: PNodeData;
		function FindHighest: PNodeData;
		procedure Remove(const K: TKey);
		function Count: Integer;
	end;

class function TSortedDictionary<TKey, TValue>.CompareNodes(D1, D2: Pointer): Integer;
begin
	Result := TComparer<TKey>.Default.Compare(PNodeData(D1)^.Key, PNodeData(D2)^.Key);
end;

constructor TSortedDictionary<TKey, TValue>.Create;
begin
	FTree := TAVLTree.Create(CompareNodes);
end;

procedure TSortedDictionary<TKey, TValue>.Clear;
var 
	Node: TAVLTreeNode;
begin
	Node := FTree.FindLowest;
	while Assigned(Node) do begin
		Dispose(PNodeData(Node.Data));
		Node := FTree.FindSuccessor(Node);
	end;
	FTree.Clear;
end;

destructor TSortedDictionary<TKey, TValue>.Destroy;
begin
	Clear;
	inherited;
end;

procedure TSortedDictionary<TKey, TValue>.Add(const K: TKey; const V: TValue);
var 
	Data: PNodeData;
	Existing: TAVLTreeNode;
begin
	New(Data); Data.Key := K; Data.Value := V;
	Existing := FTree.Find(Data);
	if Assigned(Existing) then begin
		PNodeData(Existing.Data)^.Value := V;
		Dispose(Data);
	end else
		FTree.Add(Data);
end;

function TSortedDictionary<TKey, TValue>.TryGetValue(const K: TKey; out V: TValue): Boolean;
var 
	Dummy: TNodeData;
	Node: TAVLTreeNode;
begin
	Dummy.Key := K;
	Node := FTree.Find(@Dummy);
	Result := Assigned(Node);
	if Result then V := PNodeData(Node.Data)^.Value;
end;

function TSortedDictionary<TKey, TValue>.FindLowest: PNodeData;
begin
	Result := PNodeData(FTree.FindLowest.Data);
end;

function TSortedDictionary<TKey, TValue>.FindHighest: PNodeData;
begin
	Result := PNodeData(FTree.FindHighest.Data);
end;

procedure TSortedDictionary<TKey, TValue>.Remove(const K: TKey);
var
	Dummy: TNodeData;
	Node: TAVLTreeNode;
begin
	Dummy.Key := K;
	Node := FTree.Find(@Dummy);
	if Assigned(Node) then begin
		Dispose(PNodeData(Node.Data));
		FTree.Delete(Node);
	end;
end;

function TSortedDictionary<TKey, TValue>.Count: Integer;
begin Result := FTree.Count; end;

var
	q, i, x, c, v, mn, mx: int32;
	tp: int8;
	s: TSortedDictionary<int32, int32>;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(q);

	s := TSortedDictionary<int32, int32>.Create;

	for i := 1 to q do begin
		read(tp);
		case tp of

			1: begin
				read(x);
				if not s.TryGetValue(x, v) then v := 0;
				s.Add(x, v+1);
			end;

			2: begin
				read(x, c);
				if s.TryGetValue(x, v) then begin
					if v > c then
						s.Add(x, v - c)
					else
						s.Remove(x);
				end;
			end;

			3: begin
				mn := s.FindLowest^.Key;
				mx := s.FindHighest^.Key;
				writeln(mx - mn);
			end;

		end;
		readln;
	end;

	s.Free;
end.
