# Задатак: D.pas

```pascal
program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	generics.collections;
const
	nn = 200 * 1000;
	inf = int64(1) shl 60;

type
	troad = record
		b: int32;
		c: int64;
	end;

	THeapComparer<_T> = class
		class function Compare(constref Left, Right: _T): SizeInt;
	end;

	THeap<_T, _C> = class
	private
		FItems: TList<_T>;
		function Favorite(u: SizeInt): SizeInt;
		function GetCount: SizeInt;
		{function GetItem(Index: SizeInt): _T;}
	public
		constructor Create;
		destructor Destroy; override;
		procedure Push(Item: _T);
		function Pop: _T;
		property Count: SizeInt read GetCount;
		{property Items[Index: SizeInt]: _T read GetItem; default;}
	end;

var
	n, m, t, i, a, b, c: int32;
	d: int64;
	u, v: troad;
	adj: array [1 .. nn] of tlist<troad>;
	dist: array [1 .. nn] of int64;
	pq: THeap<troad, THeapComparer<troad>>;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

function road(b: int32; c: int64): troad; inline;
begin
	result.b := b;
	result.c := c;
end;

// THeapComparer<_T>
class function THeapComparer<_T>.Compare(constref Left, Right: _T): SizeInt;
begin
	Result := Left.c - Right.c;
end;

// THeap<_T, _C>
constructor THeap<_T, _C>.Create;
begin
	Inherited Create;
	FItems := TList<_T>.Create;
	{FItems.OwnsObjects := False;}
end;

destructor THeap<_T, _C>.Destroy;
begin
	FItems.Free;
	inherited {Destroy};
end;

function THeap<_T, _C>.Favorite(u: SizeInt): SizeInt;
var
	v: SizeInt;
begin
	v := u * 2 + 2;
	if (v >= Count) or (_C.Compare(FItems[v-1], FItems[v]) < 0) then
		dec(v);
	Result := v;
end;

{function THeap<_T, _C>.GetItem(Index: SizeInt): _T;
begin
	Result := FItems[Index];
end;}

function THeap<_T, _C>.GetCount: SizeInt;
begin
	Result := FItems.Count;
end;

procedure THeap<_T, _C>.Push(Item: _T);
var
	u, v: SizeInt;
begin
	v := FItems.Count;
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
	FItems[0] := FItems[FItems.Count - 1];
	// If the Item itself needs to be freed, free it before deleting from the list
	FItems.Delete(FItems.Count - 1);

	u := 0;
	v := Favorite(u);
	while (v < FItems.Count) and (_C.Compare(FItems[v], FItems[u]) < 0) do begin
		FItems.Exchange(u, v);
		u := v;
		v := Favorite(u);
	end;
end;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, m, t);

	for a := 1 to n do adj[a] := tlist<troad>.create;

	for i := 1 to m do begin
		read(a, b, c); 
		adj[a].add(road(b, c));
		adj[b].add(road(a, c));
	end;

	for a := 1 to n do dist[a] := inf;
	dist[1] := 0;

	pq := THeap<troad, THeapComparer<troad>>.Create;
	pq.push(road(1, 0));

	while pq.count > 0 do begin
		u := pq.pop;
		if u.c = dist[u.b] then
			for v in adj[u.b] do begin
				d := dist[u.b] + v.c;
				if dist[v.b] > d then begin
					dist[v.b] := d;
					pq.push(road(v.b, d));
				end;
			end;
	end;

	if dist[t] = inf then
		writeln(-1)
	else
		writeln(dist[t] * 2);

	for a := 1 to n do adj[a].free;
	pq.Free;
end.

```
