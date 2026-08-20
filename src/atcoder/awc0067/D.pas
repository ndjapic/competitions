program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
// #dijkstra
uses
	math;
const
	nn = 400;
	mm = 400 * 200;
	inf = int64(1) shl 60;
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
	n, m, i, j, k, start, ans: int32;
	adj, c: array [1 .. nn] of int32;
	s: array [1 .. nn] of int64;
	wei: array [1 .. mm] of int64;
	d: array [1 .. nn] of int64;
	d2: array [1 .. nn, 1 .. nn] of int64;
	sib, tar: array [-mm .. mm] of int32;
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

procedure addarrow(i, j, k: int32);
begin
	sib[k] := adj[i];
	adj[i] := k;
	tar[k] := j;
end;

procedure readedges(n, m: int32);
var
	i, j, k: int32;
begin
	for i := 1 to n do begin
		read(s[i]);
		adj[i] := 0;
	end;
	readln;

	for k := 1 to m do begin
		readln(i, j, wei[k]);
		addarrow(i, j, k);
		addarrow(j, i, -k);
	end;
end;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, m);
	readedges(n, m);

	for start := 1 to n do begin

		for i := 1 to n do d[i] := inf;
		d[start] := 0;

		pq := TPrioQueue.Create();
		for i := 1 to n do pq.enqueue(i);

		while pq.n > 0 do begin
			i := pq.items[1];
			pq.dequeue(1);

			k := adj[i];
			while k <> 0 do begin
				j := tar[k];
				if d[j] > d[i] + wei[abs(k)] then begin
					d[j] := d[i] + wei[abs(k)];
					pq.swim(pq.inv[j], j);
				end;
				k := sib[k];
			end;
		end;
		pq.Free;

		for i := 1 to n do d2[start, i] := d[i];

	end;

	for i := 1 to n do begin
		c[i] := 0;
		for j := 1 to n do
			if d2[i, j] > s[i] then inc(c[i]);
	end;

	ans := 0;
	for j := 1 to n do
		for i := 1 to j - 1 do
			if (d2[i, j] <= min(s[i], s[j])) and (c[i] = c[j]) then
				inc(ans);

	writeln(ans);
end.
