program _E;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	nn = 50;
	mm = 18;
	vv = 1 shl mm;
	inf = int64(1) shl 60;
type
	TPrioQueue<_T> = class
	public
		items: array [1 .. vv] of _T;
		inv: array [1 .. vv] of int32;
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
	n, m, i, j: int8;
	u, v, eij: int32;
	c, e: array [0 .. nn] of int32;
	dist: array [0 .. vv] of int64;
	pq: TPrioQueue<int32>;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

constructor TPrioQueue<_T>.Create();
begin
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

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, m);

	for i := 0 to n-1 do read(c[i]);
	readln;

	for i := 0 to n-1 do begin
		e[i] := 0;
		for j := 0 to m-1 do begin
			read(eij);
			inc(e[i], eij shl j);
		end;
		readln;
	end;

	for v := 0 to (1 shl m) - 1 do dist[v] := inf;
	dist[0] := 0;
	pq := TPrioQueue<int32>.Create();
	for v := 0 to (1 shl m) - 1 do pq.enqueue(v);

	while pq.n > 0 do begin
		u := pq.items[1];
		pq.dequeue(1);

		for i := 0 to n-1 do begin
			v := u or e[i];
			if dist[v] > dist[u] + c[i] then begin
				dist[v] := dist[u] + c[i];
				pq.swim(pq.inv[v], v);
			end;
		end;
	end;

	v := (1 shl m) - 1;
	if dist[v] < inf then
		writeln(dist[v])
	else
		writeln(-1);

	pq.Free;
end.
