# Задатак: B.pas

```pascal
program _B;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	generics.collections, generics.defaults;
const
	nn = 200 * 1000;
type
	THeap<_T> = class
	private
		FItems: TList<_T>;
		FComparer: IComparer<_T>;
		function Favorite(u: SizeInt): SizeInt;
		function GetItem(Index: SizeInt): _T;
		function GetCount: SizeInt;
	public
		constructor Create(const AComparer: IComparer<_T>);
		destructor Destroy; override;
		procedure Push(Item: _T);
		function Pop: _T;
		property Count: SizeInt read GetCount;
		property Items[Index: SizeInt]: _T read GetItem; default;
	end;
var
	n, m, i, j, u, v: int32;
	r: array [1 .. nn] of int32;
	adj: array [1 .. nn] of TList<int32>;
	pq: THeap<int32>;
	pair: array [1 .. nn] of int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

function HeapCompare(constref Left, Right: int32): Integer;
begin
	Result := - r[Left] + r[Right];
end;

// THeap<_T> Implementation
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

constructor THeap<_T>.Create(const AComparer: IComparer<_T>);
begin
	Inherited Create;
	FItems := TList<_T>.Create;
	FComparer := AComparer;
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
	randomize;

	readln(n, m);

	pq := theap<int32>.create(TComparer<int32>.Construct(HeapCompare));
	for v := 1 to n do begin
		read(r[v]);
		pq.push(v);
		pair[v] := 0;
		adj[v] := tlist<int32>.create;
	end;
	readln;

	for j := 1 to m do begin
		readln(u, v);
		adj[u].add(v);
		adj[u].exchange(adj[u].count - 1, random(adj[u].count));
		adj[v].add(u);
		adj[v].exchange(adj[v].count - 1, random(adj[v].count));
	end;

	for i := 1 to n div 2 do begin
		u := pq.pop;
		while pair[u] > 0 do u := pq.pop;
		adj[u].sort;
		j := 0;
		v := adj[u][j];
		while pair[v] > 0 do begin
			inc(j);
			v := adj[u][j];
		end;
		pair[u] := v;
		pair[v] := u;
	end;

	writeln(pair[1]);
	pq.free;
	for v := 1 to n do adj[v].free;
end.

```
