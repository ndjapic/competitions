program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
// #default #custom #sort #heap
uses
	Generics.Collections, Generics.Defaults, Math;
type
	TFruit = record
		v, d: int32;
	end;
	THeap<_T> = class
	private
		FItems: TList<_T>;
		FComparer: IComparer<_T>;
		function Favorite(u: SizeInt): SizeInt;
		function GetItem(Index: SizeInt): _T;
		function GetCount: SizeInt;
	public
		constructor Create(AComparer: IComparer<_T>);
		destructor Destroy; override;
		procedure Push(Item: _T);
		function Pop: _T;
		property Count: SizeInt read GetCount;
		property Items[Index: SizeInt]: _T read GetItem; default;
	end;

var
	n, m, i, j, li: int32;
	ans: int64;
	fi: TFruit;
	f: Tlist<TFruit>;
	l: Tlist<int32>;
	pq: THeap<int32>;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

function FruitCompare(constref Lhs, Rhs: TFruit): int32;
begin
	Result := CompareValue(Lhs.d, Rhs.d);
end;

// HeapCompare
function HeapCompare(constref Lhs, Rhs: int32): int32;
begin
	Result := CompareValue(Rhs, Lhs);
end;

// THeap<_T>
function THeap<_T>.Favorite(u: SizeInt): SizeInt;
var
	v: SizeInt;
begin
	v := u * 2 + 2;
	if (v >= Count) or (FComparer.Compare(FItems[v-1], FItems[v]) < 0) then
		dec(v);
	Result := v;
end;

function THeap<_T>.GetItem(Index: SizeInt): _T;
begin
	Result := FItems[Index];
end;

function THeap<_T>.GetCount: SizeInt;
begin
	Result := FItems.Count;
end;

constructor THeap<_T>.Create(AComparer: IComparer<_T>);
begin
	Inherited Create;
	FItems := TList<_T>.Create;
	FComparer := AComparer;
	{FItems.OwnsObjects := False;}
end;

destructor THeap<_T>.Destroy;
begin
	FItems.Free;
	inherited Destroy;
end;

procedure THeap<_T>.Push(Item: _T);
var
	u, v: SizeInt;
begin
	v := Count;
	FItems.Add(Item);
	u := (v-1) div 2;
	while (v > 0) and (FComparer.Compare(FItems[v], FItems[u]) < 0) do begin
		FItems.Exchange(u, v);
		v := u;
		u := (v-1) div 2;
	end;
end;

function THeap<_T>.Pop: _T;
var
	u, v: SizeInt;
begin
	Result := FItems[0];
	FItems[0] := FItems[Count - 1];
	// If the Item itself needs to be freed, free it before deleting from the list
	FItems.Delete(Count - 1);

	u := 0;
	v := Favorite(u);
	while (v < Count) and (FComparer.Compare(FItems[v], FItems[u]) < 0) do begin
		FItems.Exchange(u, v);
		u := v;
		v := Favorite(u);
	end;
end;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);
	Randomize;

	readln(n, m);

	f := TList<TFruit>.Create(TComparer<TFruit>.Construct(FruitCompare));
	l := TList<int32>.Create;
	pq := THeap<int32>.Create(TComparer<int32>.Construct(HeapCompare));

	for i := 1 to n do begin
		readln(fi.d, fi.v);
		f.Add(fi);
		f.Exchange(i-1, Random(i));
	end;
	f.Sort;

	for j := 1 to m do begin
		read(li);
		l.Add(li);
		l.Exchange(j-1, Random(j));
	end;
	readln;
	l.Sort;

	ans := 0;
	i := 0;
	for li in l do begin
		while (i < n) and (f[i].d <= li) do begin
			pq.Push(f[i].v);
			inc(i);
		end;

		if pq.Count > 0 then inc(ans, pq.Pop);
	end;

	writeln(ans);
	f.Free;
	l.Free;
	pq.Free;
end.
