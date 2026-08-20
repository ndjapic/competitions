program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
// #dijkstra
uses
	generics.collections;
const
	nn = 200 * 1000;
	mm = 200 * 1000;
type
	TPrioQueue = class
	public
		items, inv: array [1 .. nn] of int32;
		n: int32;
		constructor Create();
		function prior(l, r: int32): boolean;
		procedure setItem(v, x: int32);
		procedure swim(v, x: int32);
		procedure enqueue(x: int32);
		procedure sink(u, x: int32);
		procedure dequeue(u: int32);
	end;
var
	n, m, i, k, u, v, du: int32;
	adj: array [1 .. nn] of tlist<int32>;
	d: array [1 .. nn] of int32;
	pq: TPrioQueue;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

constructor TPrioQueue.Create();
begin
	{setlength(items, 1);}
	n := 0;
end;

function TPrioQueue.prior(l, r: int32): boolean;
begin
	prior := d[l] < d[r];
end;

procedure TPrioQueue.setItem(v: int32; x: int32);
begin
	items[v] := x;
	inv[x] := v;
end;

procedure TPrioQueue.swim(v: int32; x: int32);
var
	u: int32;
begin
	u := v div 2;
	while (v > 1) and prior(x, items[u]) do begin
		setItem(v, items[u]);
		v := u;
		u := v div 2;
	end;
	setItem(v, x);
end;

procedure TPrioQueue.enqueue(x: int32);
begin
	inc(n);
	{if length(items) <= n then setlength(items, 2*n);}
	swim(n, x);
end;

procedure TPrioQueue.sink(u: int32; x: int32);
var
	v: int32;
begin
	v := u * 2;
	if (v+1 <= n) and prior(items[v+1], items[v]) then inc(v);
	while (v <= n) and prior(items[v], x) do begin
		setItem(u, items[v]);
		u := v;
		v := u * 2;
		if (v+1 <= n) and prior(items[v+1], items[v]) then inc(v);
	end;
	setItem(u, x);
end;

procedure TPrioQueue.dequeue(u: int32);
begin
	dec(n);
	sink(u, items[n+1]);
end;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, m, k);

	d[1] := 0;
	pq := TPrioQueue.Create();
	adj[1] := tlist<int32>.create;
	pq.enqueue(1);

	for v := 2 to n do begin
		adj[v] := tlist<int32>.create;
		d[v] := -1;
	end;

	for i := 1 to m do begin
		readln(u, v);
		adj[u].add(v);
		adj[v].add(u);
	end;

	while pq.n > 0 do begin
		u := pq.items[1];
		pq.dequeue(1);

		if adj[u].count < k then
			du := d[u] + 1
		else
			du := d[u] + 2;

		for v in adj[u] do
			if d[v] > du then begin
				d[v] := du;
				pq.swim(pq.inv[v], v);
			end else if d[v] < 0 then begin
				d[v] := du;
				pq.enqueue(v);
			end;
	end;

	if (d[n] < 0) or (adj[1].count < k) then
		writeln(d[n])
	else
		writeln(d[n] - 1);

	for v := 1 to n do adj[v].free;
	pq.Free;
end.
