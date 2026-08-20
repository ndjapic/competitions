program _E;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
// #default #sort #two #pointer #learn
uses
	generics.collections, generics.defaults;
var
	n, i, j, x, ai, d: int32;
	found: boolean;
	a: tlist<int32>;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);
	randomize;

	readln(n, x);
	x := abs(x);

	a := tlist<int32>.create;
	for i := 1 to n do begin
		read(ai);
		a.add(ai);
		a.exchange(i-1, random(i));
	end;
	readln;
	a.sort;

	i := 0;
	j := 0;
	found := false;

	while (i < n) and not found do begin
		d := a[i] - a[j];
		if d < x then
			inc(i)
		else if d > x then
			inc(j)
		else
			found := true;
	end;
	a.free;

	if found then
		writeln('Yes')
	else
		writeln('No');
end.
