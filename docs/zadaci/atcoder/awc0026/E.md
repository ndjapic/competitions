# Задатак: E.pas

```pascal
program _E;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	math;
const
	nn = 200 * 1000;
var
	n, k, i, l, r: integer;
	ans: int64;
	a, mx, mn: array of int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

procedure build(arr: array of int32);
var
	i: integer;
begin
	for i := 0 to n-1 do begin
		mx[n+i] := arr[i];
		mn[n+i] := arr[i];
	end;
	for i := n-1 downto 1 do begin
		mx[i] := max(mx[2*i], mx[2*i+1]);
		mn[i] := min(mn[2*i], mn[2*i+1]);
	end;
end;

function query(L, R: integer): longint;
var
	rmxq, rmnq: int32;
begin
	rmxq := Low(int32);
	rmnq := High(int32);
	inc(L, n);
	inc(R, n);
	while L <= R do begin
		if (L mod 2 = 1) then begin
			rmxq := max(rmxq, mx[L]);
			rmnq := min(rmnq, mn[L]);
		end;
		if (R mod 2 = 0) then begin
			rmxq := max(rmxq, mx[R]);
			rmnq := min(rmnq, mn[R]);
		end;
		L := (L+1) div 2;
		R := (R-1) div 2;
	end;
	query := rmxq - rmnq;
end;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, k);

	setlength(a, n);
	setlength(mx, 2*n);
	setlength(mn, 2*n);

	for i := 0 to n-1 do read(a[i]);
	readln;
	build(a);

	l := 0;
	r := 0;
	ans := 0;
	for i := 0 to n-1 do begin
		while query(l, i) > k do inc(l);
		r := max(l, r);
		while (r <= i) and (query(r, i) >= k) do inc(r);
		inc(ans, r-l);
	end;
	writeln(ans);
end.

```
