program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	nn = 200 * 1000;
	inf = 1000 * 1000 * 1000 * 1000 * 1000 * 1000;
type
	TPrioQueue<_T> = class
	public
		items: array [1 .. nn] of _T;
		inv: array [1 .. nn] of int32;
		n: int32;
		constructor Create();
		function prior(l, r: _T): boolean;
		procedure setItem(v: int32; x: _T);
		procedure swim(v: int32; x: _T);
		procedure enqueue(x: _T);
		procedure sink(u: int32; x: _T);
		procedure dequeue(u: int32);
	end;

var
	n, m, i, j, k, u, v: int32;
	d: int64;
	c, adj, wei: array [1 .. nn] of int32;
	dist: array [1 .. nn] of int64;
	sib, tar: array [-nn .. nn] of int32;
	pq: TPrioQueue<int32>;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

constructor TPrioQueue<_T>.Create();
begin
	{setlength(items, 1);}
	n := 0;
end;

function TPrioQueue<_T>.prior(l, r: _T): boolean;
begin
	prior := dist[l] < dist[r];
end;

procedure TPrioQueue<_T>.setItem(v: int32; x: _T);
begin
	items[v] := x;
	inv[x] := v;
end;

procedure TPrioQueue<_T>.swim(v: int32; x: _T);
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

procedure TPrioQueue<_T>.enqueue(x: _T);
begin
	inc(n);
	{if length(items) <= n then setlength(items, 2*n);}
	swim(n, x);
end;

procedure TPrioQueue<_T>.sink(u: int32; x: _T);
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

procedure TPrioQueue<_T>.dequeue(u: int32);
begin
	dec(n);
	sink(u, items[n+1]);
end;

procedure addarrow(u, v, i: int32);
begin
	sib[i] := adj[u];
	adj[u] := i;
	tar[i] := v;
end;

procedure readedges(n, m: int32);
var
	u, v, i: int32;
begin
	for v := 1 to n do begin
		adj[v] := 0;
		c[v] := 0;
		dist[v] := inf;
		pq.enqueue(v);
	end;

	for i := 1 to m do begin
		readln(u, v, wei[i]);
		addarrow(u, v, i);
		addarrow(v, u, -i);
	end;
end;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	pq := TPrioQueue<int32>.Create();
	readln(n, m, k);
	readedges(n, m);

	for j := 1 to k do begin
		read(v);
		readln(c[v]);
	end;

	dist[1] := c[1];
	pq.swim(pq.inv[1], 1);

	while pq.n > 0 do begin

		u := pq.items[1];
		pq.dequeue(1);

		i := adj[u];
		while i <> 0 do begin
			v := tar[i];
			d := dist[u] + wei[abs(i)] + c[v];
			if dist[v] > d then begin
				dist[v] := d;
				pq.swim(pq.inv[v], v);
			end;
			i := sib[i];
		end;

	end;

	writeln(dist[n]);
	pq.Free;
end.
