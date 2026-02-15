program _E;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	Generics.Collections, Generics.Defaults;
const
	nn = 40;
var
	n, e, h: int8;
	i, j, ans: int32;
	x, s: int64;
	a: array [0 .. nn] of int64;
	l, r: TList<int64>;
	link: array of int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, s);
	h := (n+1) div 2;

	for e := 0 to n-1 do read(a[e]);
	readln;

	l := TList<int64>.Create;
	l.Add(0);
	for e := 0 to h-1 do begin
		i := l.Count - 1;
		while i >= 0 do begin
			x := l[i] + a[e];
			if x <= s then l.Add(x);
			dec(i);
		end;
	end;
	l.Sort;

	r := TList<int64>.Create;
	r.Add(0);
	for e := h to n-1 do begin
		j := r.Count - 1;
		while j >= 0 do begin
			x := r[j] + a[e];
			if x <= s then r.Add(x);
			dec(j);
		end;
	end;
	r.Sort;

	setlength(link, r.Count);
	link[0] := -1;
	for j := 1 to r.Count - 1 do
		if r[j-1] < r[j] then
			link[j] := j-1
		else
			link[j] := link[j-1];

	ans := 0;
	j := r.Count - 1;
	for i := 0 to l.Count - 1 do begin
		while (j >= 0) and (l[i] + r[j] > s) do j := link[j];
		if (j >= 0) and (l[i] + r[j] = s) then inc(ans, j - link[j]);
	end;

	writeln(ans);
	l.Free;
	r.Free;
end.
