program B_Most_Frequent_Substrings;
{$MODE DELPHI}
uses
	math;
const
	nn = 100;
var
	n, k, i, j, len, p, x: int8;
	s: string;
	a, cp, c, ans: array [1 .. nn] of int8;

function leq(l, r: int8): boolean;
var
	i: int8;
begin
	i := 0;
	while (i < k-1) and (s[l+i] = s[r+i]) do inc(i);
	leq := s[l+i] <= s[r+i];
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
			if (r = rend) or (l < m) and leq(a[l], a[r]) then begin
				cp[i] := a[l];
				inc(l);
			end else begin
				cp[i] := a[r];
				inc(r);
			end;

		for i := lend to rend - 1 do a[i] := cp[i];
	end;
end;

begin
	readln(n, k);
	readln(s);

	for i := 1 to n+1-k do a[i] := i;
	MergeSort(1, n+2-k);

	c[1] := 1;
	x := 1;
	for i := 2 to n+1-k do begin
		if leq(a[i], a[i-1]) then
			c[i] := c[i-1] + 1
		else
			c[i] := 1;
		x := max(x, c[i]);
	end;

	j := 0;
	for i := 1 to n+1-k do
		if c[i] = x then begin
			inc(j);
			ans[j] := i;
		end;
	len := j;

	writeln(x);
	for j := 1 to len do begin
		i := ans[j];
		for p := a[i] to a[i] - 1 + k do write(s[p]);
		if j < len then write(' ');
	end;
	writeln;
end.
