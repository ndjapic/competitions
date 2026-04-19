# Problem: Problem_A_Warm_Up.pas

```pascal
program Problem_A_Warm_Up;
{$MODE DELPHI}
uses
	Generics.Defaults, Generics.Collections, math;
const
	nn = 500 * 1000;
type
	tpair = record
		i, j: int32;
	end;
var
	ntc, tci, n, i, j, k, t: int32;
	ans: boolean;
	p: tpair;
	a, b: array [1 .. nn] of int32;
	inda, indb: array [1 .. nn] of TList<int32>;
	op: TList<tpair>;

begin
	readln(ntc);
	for tci := 1 to ntc do begin

		readln(n);

		for t := 1 to n do begin
			inda[t] := TList<int32>.Create;
			indb[t] := TList<int32>.Create;
		end;

		op := TList<tpair>.Create;

		try

			for j := 1 to n do begin
				read(t);
				a[j] := t;
				inda[t].Add(j);
			end;
			readln;

			for i := 1 to n do begin
				read(t);
				b[i] := t;
				indb[t].Add(i);
			end;
			readln;

			ans := true;
			for t := 1 to n do
				for k := 0 to inda[t].Count - 1 do
					if ans then begin
						p.j := inda[t][k];
						if a[p.j] > b[p.j] then
							ans := false
						else if a[p.j] < b[p.j] then begin
							ans := inda[b[p.j]].Count > 0;
							if ans then begin
								p.i := inda[b[p.j]][0];
								op.Add(p);
							end;
						end;
					end;

			write('Case #', tci, ': ');
			if ans then begin
				writeln(op.Count);
				for k := 0 to op.Count - 1 do
					writeln(op[k].i, ' ', op[k].j);
			end else
				writeln(-1);

		finally

			for t := 1 to n do begin
				inda[t].Free;
				indb[t].Free;
			end;

			op.Free;

		end;

	end;
end.

```
