program _E;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	Generics.Collections, Generics.Defaults, SysUtils, Math;
const
	NN = 200 * 1000;
type
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
	n, i: int32;
	s: int64;
	a: array [1 .. NN] of int32;
	ans: array [1 .. NN] of int64;
	p: TList<int32>;
	pq: THeap<int32>;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

function ListCompare(constref l, r: int32): int32;
begin
	Result := CompareValue(a[l], a[r]);
end;

function HeapCompare(constref l, r: int32): int32;
begin
	Result := CompareValue(l, r);
end;
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

	readln(n);

	s := 0;
	p := TList<int32>.Create;
	pq := THeap<int32>.Create(TComparer<int32>.Construct(HeapCompare));

	for i := 1 to n do begin
		read(a[i]);
		inc(s, a[i]);
		p.Add(i);
		p.Exchange(i-1, Random(i));
		pq.Push(a[i]);
	end;
	readln;
	p.Sort(TComparer<int32>.Construct(ListCompare));

	for i in p do begin
		while (pq.Count > 0) and (pq[0] <= a[i]) do dec(s, pq.Pop);
		ans[i] := s;
	end;

	for i := 1 to n-1 do write(ans[i], ' ');
	writeln(ans[n]);

	p.Free;
	pq.Free;
end.
