# Задатак: D.pas

```pascal
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
	n, m, k, i, u, v: int32;
	adj: array [1 .. nn] of int32;
	wei, d: array [1 .. nn] of int64;
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
	prior := d[l] < d[r];
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

procedure readedges(n, m, k: int32);
var
	u, v, i, j: int32;
begin
	for v := 1 to n do adj[v] := 0;

	for i := 1 to m do begin
		readln(u, v, wei[i]);
		addarrow(u, v, i);
		addarrow(v, u, -i);
	end;

	for j := 1 to k do begin
		read(i);
		inc(wei[i], wei[i]);
	end;
	if k > 0 then readln;
end;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, m, k);
	readedges(n, m, k);

	for v := 1 to n do d[v] := inf;
	d[1] := 0;
	pq := TPrioQueue<int32>.Create();
	for v := 1 to n do pq.enqueue(v);

	while pq.n > 0 do begin

		u := pq.items[1];
		pq.dequeue(1);

		i := adj[u];
		while i <> 0 do begin
			v := tar[i];
			if d[v] > d[u] + wei[abs(i)] then begin
				d[v] := d[u] + wei[abs(i)];
				pq.swim(pq.inv[v], v);
			end;
			i := sib[i];
		end;

	end;

	if d[n] = inf then d[n] := -1;
	writeln(d[n]);
	pq.Free;
end.

```
