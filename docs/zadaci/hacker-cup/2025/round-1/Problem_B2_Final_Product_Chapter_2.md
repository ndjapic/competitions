# Задатак: Problem_B2_Final_Product_Chapter_2.pas

```pascal
program Problem_B2_Final_Product_Chapter_2;
uses
	math;
const
	nn = 10 * 1000 * 1000;
	dd = 10 * 1000;
	prime = 1000 * 1000 * 1000 + 7;
var
	ntc, tci, n, i, a, b, p: int32;
	t: int16;
	lpd: array [2 .. nn] of int32;
	divs, cp: array [1 .. dd] of int64;

function isqrt(a: int64): int64;
var
	x: int64;
begin
	x := min(a, high(int32));
	while x * x > a do
		x := (x + a div x) div 2;
	isqrt := x;
end;

function get_lpd(n: int64): int64;
var
	p, s: int64;
begin
	if n <= nn then
		get_lpd := lpd[n]
	else begin
		s := isqrt(n);
		if (lpd[s] = s) and (s*s = n) then
			get_lpd := s
		else begin
			p := 2;
			while (p <= s) and ((lpd[p] < p) or (n div p > 0)) do inc(p);
			if p <= s then
				get_lpd := p
			else
				get_lpd := n;
		end;
	end;
end;

procedure get_divs(x: int64);
var
	p: int64;
	i, t0: int16;
begin
	while x > 1 do begin
		t0 := t;
		p := get_lpd(x);
		while x mod p = 0 do begin
			x := x div p;
			for i := 1 to t0 do begin
				inc(t);
				divs[t] := divs[t-t0] * p;
			end;
		end;
		get_divs(x);
	end;
end;

procedure MergeSort(lend, rend: int8);
var
    i, l, r, m: int8;

begin
	if rend - lend > 1 then begin
		m := (lend + rend) div 2;
		MergeSort(lend, m);
		MergeSort(m, rend);

		l := lend;
		r := m;
		for i := lend to rend - 1 do
			if (r = rend) or (l < m) and (divs[l] <= divs[r]) then begin
				cp[i] := divs[l];
				inc(l);
			end else begin
				cp[i] := divs[r];
				inc(r);
			end;

		for i := lend to rend - 1 do divs[i] := cp[i];
	end;
end;

begin
	for n := 2 to nn do lpd[n] := n;

	for p := 2 to nn do
		if lpd[p] = p then
			for i := p to nn div p do begin
				n := i * p;
				if lpd[n] = n then lpd[n] := p;
			end;

	readln(ntc);
	for tci := 1 to ntc do begin

		readln(n, a, b);

		t := 1;
		divs[1] := 1;
		get_divs(b);
		MergeSort(0, t);

		write('Case #', tci, ': ');
		for i := 1 to 2*n-1 do write('1 ');
		writeln(b);

	end;
end.

```
