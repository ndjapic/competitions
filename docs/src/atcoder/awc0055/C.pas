program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	generics.collections,
	generics.defaults;
var
	n, i, j, elm: int32;
	w, c: tlist<int32>;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);
	randomize;

	readln(n);
	w := tlist<int32>.create;
	c := tlist<int32>.create;
	try

		for i := 0 to n-1 do begin
			read(elm);
			w.add(elm);
			w.exchange(i, random(i+1));
		end;
		readln;
		w.sort;

		for j := 0 to n-1 do begin
			read(elm);
			c.add(elm);
			c.exchange(j, random(j+1));
		end;
		readln;
		c.sort;

		i := 0;
		for j := 0 to n-1 do
			if w[i] <= c[j] then inc(i);

		writeln(i);

	finally
		w.free;
		c.free;
	end;
end.
