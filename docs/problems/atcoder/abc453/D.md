# Problem: D.pas

```pascal
program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	generics.collections, generics.defaults;
const
	hh = 1000;
	nn = 4000 * 1000;
	inf = 1000 * 1000 * 1000;

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
	h, w, n, i, j, k, u, v, si, sj, gi, gj, e, f: int32;
	s: array [1 .. hh] of string;
	d, par, rev: array [1 .. nn] of int32;
	pq: TPrioQueue<int32>;

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

function vert(i, j, e: int32): int32;
begin
	result := (e*h + i-1) * w + j;
end;

function geti(v: int32): int32;
begin
	result := (v-1) mod (h*w) div w + 1;
end;

function getj(v: int32): int32;
begin
	result := (v-1) mod (h*w) mod w + 1;
end;

function gete(v: int32): int32;
begin
	result := (v-1) div (h*w);
end;

procedure deckey(u, i, j, e: int32);
var
	v: int32;
begin
	v := vert(i, j, e);
	if (s[i][j] <> '#') and (d[v] > d[u] + 1) then begin
		par[v] := u;
		d[v] := d[u] + 1;
		pq.swim(pq.inv[v], v);
	end;
end;

begin
	readln(h, w);
	n := 4*h*w;

	for v := 1 to n do d[v] := inf;

	for i := 1 to h do begin
		readln(s[i]);
		for j := 1 to w do
			if s[i][j] = 'S' then begin
				si := i;
				sj := j;
				for e := 0 to 3 do d[vert(i, j, e)] := 0;
			end else if s[i][j] = 'G' then begin
				gi := i;
				gj := j;
			end;
	end;

	pq := TPrioQueue<int32>.Create();
	for v := 1 to n do pq.enqueue(v);

	while pq.n > 0 do begin
		u := pq.items[1];
		pq.dequeue(1);
		i := geti(u);
		j := getj(u);
		e := gete(u);

		for f := 0 to 3 do
			if (s[i][j] = 'o') and (e = f) or
				(s[i][j] = 'x') and (e <> f) or
				(s[i][j] <> 'o') and (s[i][j] <> 'x')
			then case f of
				0: if i > 1 then deckey(u, i-1, j, f);
				1: if i < h then deckey(u, i+1, j, f);
				2: if j > 1 then deckey(u, i, j-1, f);
				3: if j < w then deckey(u, i, j+1, f);
			end;
	end;

	e := 0;
	for f := 0 to 3 do
		if d[vert(gi, gj, f)] < d[vert(gi, gj, e)] then e := f;
	v := vert(gi, gj, e);

	if d[v] >= inf then
		writeln('No')
	else begin
		writeln('Yes');

		k := 1;
		rev[k] := v;
		while (geti(rev[k]) <> si) or (getj(rev[k]) <> sj) do begin
			inc(k);
			rev[k] := par[rev[k-1]];
		end;

		for i := k-1 downto 1 do
			case gete(rev[i]) of
				0: write('U');
				1: write('D');
				2: write('L');
				3: write('R');
			end;
		writeln;
	end;

	pq.Free;
end.

```
