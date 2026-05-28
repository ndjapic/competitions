program _E;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
// #default #sort #list
uses
	generics.collections,
	generics.defaults;
var
	n, m, i, j, k, w, rob: int32;
	h, b: tlist<int32>;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

function readsorted(n: int32): tlist<int32>;
var
	i: int32;
begin
	result := tlist<int32>.create;
	for i := 1 to n do begin
		read(w);
		result.add(w);
		result.exchange(i-1, random(i));
	end;
	readln;
	result.sort;
end;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);
	randomize;

	readln(n, m, k);
	h := readsorted(n);
	b := readsorted(m);

	rob := 0;
	i := 0;
	j := 0;

	while (i < n) and (j < m) and (rob < k) do begin
		if h[i] <= b[j] then begin
			inc(rob);
			inc(i);
		end;
		inc(j);
	end;

	if rob < k then
		writeln('No')
	else
		writeln('Yes');

	h.free;
	b.free;
end.
