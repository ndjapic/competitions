program _E;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
// #heap #set
uses
	Generics.Collections, Generics.Defaults, SysUtils, Math;
const
	NN = 300 * 1000;
type
	THeap<T> = class
	private
		FList: TList<T>;
		FComparer: IComparer<T>;
		function GetFavoriteChild(ParentIdx: int32): int32;
	public
		constructor Create(AComparer: IComparer<T>);
		destructor Destroy; override;
		procedure Push(const Item: T);
		function Pop: T;
		function Count: int32;
	end;
var
	n, q, i, j, k, iq, bi: int32;
	a: array [1 .. NN] of int32;
	pq: THeap<Int32>;
	b: TDictionary<int32, boolean>;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

function HeapCompare(constref l, r: Int32): int32;
begin
	Result := CompareValue(a[l], a[r]);
end;

constructor THeap<T>.Create(AComparer: IComparer<T>);
begin
	FList := TList<T>.Create;
	FComparer := AComparer;
end;

destructor THeap<T>.Destroy;
begin
	FList.Free;
	inherited;
end;

function THeap<T>.Count: int32;
begin
	Result := FList.Count;
end;

function THeap<T>.GetFavoriteChild(ParentIdx: int32): int32;
var
	L, R: int32;
begin
	L := ParentIdx * 2 + 1;
	R := L + 1;
	Result := L;
	if (R < FList.Count) and (FComparer.Compare(FList[R], FList[L]) < 0) then
		Result := R;
end;

procedure THeap<T>.Push(const Item: T);
var
	Idx, ParentIdx: int32;
begin
	Idx := FList.Add(Item);
	ParentIdx := (Idx - 1) div 2;
	while (Idx > 0) and (FComparer.Compare(FList[Idx], FList[ParentIdx]) < 0) do begin
		FList.Exchange(Idx, ParentIdx);
		Idx := ParentIdx;
		ParentIdx := (Idx - 1) div 2;
	end;
end;

function THeap<T>.Pop: T;
var
	Idx, ChildIdx: int32;
begin
	Result := FList[0];
	FList[0] := FList[FList.Count - 1];
	FList.Delete(FList.Count - 1);
	
	Idx := 0;
	ChildIdx := GetFavoriteChild(Idx);
	while (ChildIdx < FList.Count) and (FComparer.Compare(FList[ChildIdx], FList[Idx]) < 0) do begin
		FList.Exchange(Idx, ChildIdx);
		Idx := ChildIdx;
		ChildIdx := GetFavoriteChild(Idx);
	end;
end;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	pq := THeap<int32>.Create(TComparer<Int32>.Construct(HeapCompare));
	b := TDictionary<int32, boolean>.Create;

	readln(n, q);

	for i := 1 to n do begin
		read(a[i]);
		pq.Push(i);
	end;
	readln;

	for iq := 1 to q do begin
		readln(k);
		b.Clear;
		for j := 1 to k do begin
			read(bi);
			b.AddOrSetValue(bi, true);
		end;
		readln;

		i := pq.Pop;
		while b.ContainsKey(i) do
			i := pq.Pop;

		writeln(a[i]);
		pq.Push(i);
		for bi in b.Keys do pq.Push(bi);
	end;

	pq.Free;
	b.Free;
end.
