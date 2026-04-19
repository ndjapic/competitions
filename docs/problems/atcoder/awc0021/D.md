# Problem: D.pas

```pascal
program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	Generics.Collections;
const
	nn = 50 * 1000;
	kk = 11;
	inf = int64(1) shl 60;
type
	TRoad = record
		t: int32;
		v: int32;
	end;
	TPath = record
		d: int64;
		v: int32;
	end;
	THeapComparer<_T> = class
		function Compare(constref Left, Right: _T): SizeInt;
	end;
	THeap<_T> = class
	private
		FItems: TList<_T>;
		function Favorite(u: SizeInt): SizeInt;
		function GetItem(Index: SizeInt): _T;
		function GetCount: SizeInt;
	public
		constructor Create;
		destructor Destroy; override;
		procedure Push(Item: _T);
		function Pop: _T;
		property Count: SizeInt read GetCount;
		property Items[Index: SizeInt]: _T read GetItem; default;
	end;

var
	n, m, i, u, v: int32;
	k, j: int8;
	road: TRoad;
	path: TPath;
	adj: array [1 .. nn] of TList<TRoad>;
	p: array [0 .. kk] of int32;
	dist: array [1 .. nn] of int64;
	Comparer: THeapComparer<TPath>;
	pq: THeap<TPath>;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

// Implementacija metode THeapComparer<_T>.Compare
function THeapComparer<_T>.Compare(constref Left, Right: _T): SizeInt;
begin
	Result := Left.d - Right.d;
end;

// Implementacija metoda THeap<_T>
function THeap<_T>.Favorite(u: SizeInt): SizeInt;
var
	v: SizeInt;
begin
	v := u * 2 + 2;
	if (v >= Count) or (Comparer.Compare(FItems[v-1], FItems[v]) < 0) then
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

constructor THeap<_T>.Create;
begin
	Inherited Create;
	FItems := TList<_T>.Create;
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
	while (v > 0) and (Comparer.Compare(FItems[v], FItems[u]) < 0) do begin
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
	while (v < Count) and (Comparer.Compare(FItems[v], FItems[u]) < 0) do begin
		FItems.Exchange(u, v);
		u := v;
		v := Favorite(u);
	end;
end;

// Glavni programski blok
begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, m, k);

	for v := 1 to n do adj[v] := TList<TRoad>.Create;
	for i := 1 to m do begin
		readln(u, v, road.t);
		road.v := v;
		adj[u].Add(road);
		road.v := u;
		adj[v].Add(road);
	end;

	dist[1] := 0;
	dist[n] := inf;
	p[0] := 1;
	p[k+1] := n;
	for j := 1 to k do read(p[j]); readln;
	Comparer := THeapComparer<TPath>.Create;

	for j := 0 to k do
		if dist[p[j]] < inf then begin
			path.v := p[j];
			path.d := dist[p[j]];
			pq := THeap<TPath>.Create;
			pq.Push(path);
			for v := 1 to n do
				if v <> p[j] then dist[v] := inf;

			while pq.Count > 0 do begin
				path := pq.Pop;
				u := path.v;

				if dist[u] = path.d then
					for road in adj[u] do begin
						path.v := road.v;
						inc(path.d, road.t);

						if dist[path.v] > path.d then begin
							dist[path.v] := path.d;
							pq.Push(path);
						end;

						dec(path.d, road.t);
					end;
			end;

			pq.Free;
		end;

	if dist[n] >= inf then dist[n] := -1;
	writeln(dist[n]);

	Comparer.Free;
	for v := 1 to n do adj[v].Free;
end.

```
