program D_Traffic_Lights;
{$mode objfpc}
const
	nn = 5000;
	inf = 1000 * 1000 * 1000;

type
	TPrioQueue = class
	public
		items: array [1 .. nn] of int32;
		inv: array [1 .. nn] of int32;
		n: int32;
		constructor Create();
		function prior(l, r: int32): boolean;
		procedure setItem(v: int32; x: int32);
		procedure swim(v: int32; x: int32);
		procedure enqueue(x: int32);
		procedure sink(u: int32; x: int32);
		procedure dequeue(u: int32);
	end;

var
	ntc, tci, n, m, u, v, e, t: int32;
	adj: array [1 .. nn, 1 .. nn] of int32;
	par, deg, d, wt: array [1 .. nn] of int32;
	pq: TPrioQueue;

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

procedure addarrow(u, v: int32);
begin
	inc(deg[u]);
	adj[u, deg[u]] := v;
end;

procedure readedges(n, m: int32);
var
	u, v, e: int32;
begin
	for v := 1 to n do begin
		deg[v] := 0;
		wt[v] := 0;
	end;

	for e := 1 to m do begin
		readln(u, v);
		addarrow(u, v);
		addarrow(v, u);
	end;
end;

begin
	readln(ntc);
	for tci := 1 to ntc do begin

		readln(n, m);
		readedges(n, m);

		for v := 1 to n do d[v] := inf;
		d[1] := 0;
		pq := TPrioQueue.Create();
		for v := 1 to n do pq.enqueue(v);

		while pq.n > 0 do begin

			u := pq.items[1];
			pq.dequeue(1);

			if u < n then begin

				for e := 1 to deg[u] do begin
					v := adj[u, (d[u] + e-1) mod deg[u] + 1];
					if (d[v] > d[u] + e) or (d[v] = d[u] + e) and (wt[u] > e-1) then begin
						par[v] := u;
						d[v] := d[u] + e;
						wt[u] := e-1;
						pq.swim(pq.inv[v], v);
					end;
				end;
			end;

		end;

		v := n;
		t := 0;
		while v > 1 do begin
			v := par[v];
			inc(t, wt[v]);
		end;

		writeln(d[n], ' ', t);

	end;
end.
