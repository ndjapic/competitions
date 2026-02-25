program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	generics.collections;
const
	nn = 500;
type
	tcell = record
		i, j: int16;
	end;
	THeapComparer<_T> = class
		class function Compare(constref l, r: _T): int16;
	end;
	THeap<_T, _C> = class
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
	n, m, i, j, nm, d: int16;
	s: array [1 .. nn] of string;
	dist: array [1 .. nn*nn] of int16;
	u, v: tcell;
	pq: THeap<tcell, THeapComparer<tcell>>;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

function ij(i, j: int32): int32;
begin
	ij := (i-1) * m + j;
end;

// THeapComparer<_T>
class function THeapComparer<_T>.Compare(constref l, r: _T): int16;
begin
	Result := dist[ij(l.i, l.j)] - dist[ij(r.i, r.j)];
end;

// THeap<_T, _C>
function THeap<_T, _C>.Favorite(u: SizeInt): SizeInt;
var
	v: SizeInt;
begin
	v := u * 2 + 2;
	if (v >= Count) or (_C.Compare(FItems[v-1], FItems[v]) < 0) then
		dec(v);
	Result := v;
end;

function THeap<_T, _C>.GetItem(Index: SizeInt): _T;
begin
	Result := FItems[Index];
end;

function THeap<_T, _C>.GetCount: SizeInt;
begin
	Result := FItems.Count;
end;

constructor THeap<_T, _C>.Create;
begin
	Inherited Create;
	FItems := TList<_T>.Create;
	{FItems.OwnsObjects := False;}
end;

destructor THeap<_T, _C>.Destroy;
begin
	FItems.Free;
	inherited Destroy;
end;

procedure THeap<_T, _C>.Push(Item: _T);
var
	u, v: SizeInt;
begin
	v := Count;
	FItems.Add(Item);
	u := (v-1) div 2;
	while (v > 0) and (_C.Compare(FItems[v], FItems[u]) < 0) do begin
		FItems.Exchange(u, v);
		v := u;
		u := (v-1) div 2;
	end;
end;

function THeap<_T, _C>.Pop: _T;
var
	u, v: SizeInt;
begin
	Result := FItems[0];
	FItems[0] := FItems[Count - 1];
	// If the Item itself needs to be freed, free it before deleting from the list
	FItems.Delete(Count - 1);

	u := 0;
	v := Favorite(u);
	while (v < Count) and (_C.Compare(FItems[v], FItems[u]) < 0) do begin
		FItems.Exchange(u, v);
		u := v;
		v := Favorite(u);
	end;
end;

procedure enqueue(i, j, d: int16);
begin
	if s[i][j] = '#' then inc(d);
	if dist[ij(i, j)] > d then begin
		dist[ij(i, j)] := d;
		v.i := i;
		v.j := j;
		pq.push(v);
	end;
end;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, m);
	nm := n + m;

	for i := 1 to n do begin
		readln(s[i]);
		for j := 1 to m do dist[ij(i, j)] := nm;
	end;
	dist[1] := 0;

	pq := THeap<tcell, THeapComparer<tcell>>.create;
	v.i := 1;
	v.j := 1;
	pq.push(v);

	while pq.count > 0 do begin

		u := pq.pop;
		d := dist[ij(u.i, u.j)];

		if u.i > 1 then enqueue(u.i-1, u.j, d);
		if u.i < n then enqueue(u.i+1, u.j, d);
		if u.j > 1 then enqueue(u.i, u.j-1, d);
		if u.j < m then enqueue(u.i, u.j+1, d);

	end;

	writeln(dist[n*m]);
	pq.free;
end.
