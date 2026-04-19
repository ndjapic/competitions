# Problem: E.pas

```pascal
program E;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	math, Generics.Collections, Generics.Defaults;
const
	nn = 200 * 1000 + 1;
var
	notc, tci, n, m, k, i, j, x, t, alive: int32;
	a, b: TList<int32>;
	died: array [1 .. nn] of int32;
	s: string;
	ttl: array [-nn .. nn] of int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);
	readln(notc);
	for tci := 1 to notc do begin

		readln(n, m, k);

		a := TList<int32>.Create;
		for i := 0 to n-1 do begin
			read(x);
			a.Add(x);
		end;
		readln;
		a.Sort;

		b := TList<int32>.Create;
		for j := 0 to m-1 do begin
			read(x);
			b.Add(x);
		end;
		readln;
		b.Sort;

		readln(s);

		for x := -k to k do ttl[x] := k+1;

		x := 0;
		for t := 1 to k do begin
			case s[t] of
				'L': dec(x);
				'R': inc(x);
			end;
			ttl[x] := min(ttl[x], t);
			died[t] := 0;
		end;

		j := 0;
		for i := 0 to n-1 do begin
			while (j < m) and (b[j] < a[i]) do inc(j);
			t := k+1;
			if (j > 0) and (b[j-1] - a[i] >= -k) then t := min(t, ttl[ b[j-1] - a[i] ]);
			if (j < m) and (b[j] - a[i] <= k) then t := min(t, ttl[ b[j] - a[i] ]);
			inc(died[t]);
		end;

		alive := n;
		for t := 1 to k do begin
			dec(alive, died[t]);
			write(alive);
			if t < k then write(' ');
		end;
		writeln;

		a.Free;
		b.Free;

	end;
end.

```
