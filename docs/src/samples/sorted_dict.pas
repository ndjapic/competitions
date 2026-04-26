program sorted_dict_avl;
{$MODE DELPHI}
uses
	AVL_Tree, Generics.Defaults, SysUtils, Math;
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
		// Static metoda nema 'Self' i kompatibilna je sa TAVLTree
		class function CompareNodes(D1, D2: Pointer): Integer; static;
	public
		constructor Create;
		destructor Destroy; override;
		procedure Add(const K: TKey; const V: TValue);
		function TryGetValue(const K: TKey; out V: TValue): Boolean;
		procedure Remove(const K: TKey);
		procedure Clear;
		function Count: Integer;
	end;

class function TSortedDictionary<TKey, TValue>.CompareNodes(D1, D2: Pointer): Integer;
begin
	// Koristimo podrazumevani komparator za tip TKey
	Result := TComparer<TKey>.Default.Compare(PNodeData(D1)^.Key, PNodeData(D2)^.Key);
end;

constructor TSortedDictionary<TKey, TValue>.Create;
begin
	FTree := TAVLTree.Create(CompareNodes);
end;

destructor TSortedDictionary<TKey, TValue>.Destroy;
var Node: TAVLTreeNode;
begin
	Node := FTree.FindLowest;
	while Assigned(Node) do begin
		Dispose(PNodeData(Node.Data));
		Node := FTree.FindSuccessor(Node);
	end;
	FTree.Free;
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

procedure TSortedDictionary<TKey, TValue>.Remove(const K: TKey);
var
	Dummy: TNodeData;
	Node: TAVLTreeNode;
begin
	Dummy.Key := K;
	Node := FTree.Find(@Dummy);
	if Assigned(Node) then begin
		// 1. Прво ослобађамо податке које смо алоцирали са New(Data)
		Dispose(PNodeData(Node.Data));
		// 2. Затим уклањамо сам чвор из стабла
		FTree.Delete(Node);
	end;
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
	FTree.Clear; // Сада је стабло потпуно празно и спремно за нове податке
end;

function TSortedDictionary<TKey, TValue>.Count: Integer;
begin Result := FTree.Count; end;

var
	Dict: TSortedDictionary<Int64, string>;
	Val: string;
begin
	Dict := TSortedDictionary<Int64, string>.Create;
	Dict.Add(10, 'Deset');
	Dict.Add(5, 'Pet');
	if Dict.TryGetValue(5, Val) then Writeln('Kljuc 5 = ', Val);
	Dict.Free;
end.
