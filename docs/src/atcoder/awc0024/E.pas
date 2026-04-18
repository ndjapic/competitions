program _E;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	Generics.Defaults, Generics.Collections, Math;
const
	nn = 100;
	ww = 50 * 1000;
type
	TString = record
		l, c: int32;
	end;
	THeapNode = record
		d, v: int32;
	end;
	THeapComparer<_T> = class
		class function Compare(constref Left, Right: _T): SizeInt;
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

// THeapComparer<_T>
class function THeapComparer<_T>.Compare(constref Left, Right: _T): SizeInt;
begin
	Result := Left.d - Right.d;
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

function CompareStrings(constref Left, Right: TString): Integer;
begin
	Result := Right.l - Left.l;
	if Result = 0 then
		Result := Right.c - Left.c;
end;

function gcd(a, b: int32): int32;
begin
    if b = 0 then
        gcd := a
    else
        gcd := gcd(b, a mod b);
end;

var
	n, i: int8;
	w, k, u: int32;
	h: THeapNode;
	si: TString;
	dist: array [0 .. ww] of int32;
	g: array [0 .. nn] of int32;
	s: TList<TString>;
	pq: THeap<THeapNode, THeapComparer<THeapNode>>;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, w);

	s := TList<Tstring>.Create;
	g[0] := 0;
	for i := 1 to n do begin
		readln(si.l, si.c);
		s.Add(si);
		g[i] := gcd(g[i-1], si.l);
	end;
	s.Sort(TComparer<TString>.Construct(CompareStrings));

	for u := 1 to w do dist[u] := w+1;
	dist[0] := 0;

	pq := THeap<THeapNode, THeapComparer<THeapNode>>.Create;
	h.d := 0;
	h.v := 0;
	pq.Push(h);

	if w mod g[n] = 0 then
		while pq.Count > 0 do begin
			h := pq.Pop;
			u := h.v;

			if h.d = dist[u] then
				for si in s do begin
					k := 1;
					h.v := u + si.l;

					while (k <= si.c) and (h.v <= w) do begin
						h.d := dist[u] + k;
						if dist[h.v] > h.d then begin
							dist[h.v] := h.d;
							pq.Push(h);
						end;
						inc(h.v, si.l);
						inc(k);
					end;
				end;
		end;

	if dist[w] > w then
		writeln(-1)
	else
		writeln(dist[w]);

	s.Free;
	pq.Free;
end.
