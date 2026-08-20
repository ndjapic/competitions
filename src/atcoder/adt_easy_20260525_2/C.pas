program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
// #min #segtree #walk #point #update
uses
	Generics.Collections;
const
	NN = 100;
	TT = 256;
var
	n, q, i, b, l, r: int8;
	v: int32;
	c: array [1 .. NN] of int8;
	st: array [1 .. TT] of int8;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

procedure combine(v: int32);
begin
	if c[st[2*v]] <= c[st[2*v+1]] then
		st[v] := st[2*v]
	else
		st[v] := st[2*v+1];
end;

procedure build(v, l, r: int32);
var
	m: int32;
begin
	if l < r then begin
		m := (l+r) div 2;
		build(2*v, l, m);
		build(2*v+1, m+1, r);
	end;
	st[v] := l;
end;

procedure update(v, vl, vr, b: int32);
var
	m: int32;
begin
	if (b < vl) or (vr < b) then
	else if (b <= vl) and (vr <= b) then
		st[v] := b
	else {if vl < vr then} begin
		m := (vl+vr) div 2;
		update(2*v, vl, m, b);
		update(2*v+1, m+1, vr, b);
		combine(v);
	end;
end;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, q);
	for b := 1 to n do c[b] := 0;
	build(1, 1, n);

	for i := 1 to q do begin
		read(b);

		if b = 0 then begin
			v := 1;
			l := 1;
			r := n;
			while l < r do begin
				b := (l+r) div 2;
				if c[st[2*v]] <= c[st[2*v+1]] then begin
					r := b;
					v := 2*v;
				end else begin
					l := b+1;
					v := 2*v+1;
				end;
			end;
			b := l;
		end;

		inc(c[b]);
		update(1, 1, n, b);
		write(b);
		if i < q then write(' ');
	end;
	readln;
	writeln;
end.
