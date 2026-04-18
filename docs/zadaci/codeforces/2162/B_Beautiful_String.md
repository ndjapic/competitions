# Задатак: B_Beautiful_String.pas

```pascal
program B_Beautiful_String;
{$MODE DELPHI}
uses
	math, Generics.Collections;
var
	notc, tci, n: int32;
	found: boolean;
	s: string;
	p, x: TList<int32>;

procedure dfs(i: int32);
var
	j, k, l, r: int32;
begin
	if found then
	else if (i <= n) then begin

		x.Add(i);
		dfs(i+1);
		x.Delete(x.Count - 1);

		if not found then begin
			p.Add(i);
			dfs(i+1);
			p.Delete(p.Count - 1);
		end;

	end else begin

		j := 1;
		k := p.Count;
		while (j < k) and (s[p[j-1]] <= s[p[j]]) do inc(j);

		l := 0;
		r := x.Count - 1;
		while (l < r) and (s[x[l]] = s[x[r]]) do begin
			inc(l);
			dec(r);
		end;

		found := (j = k) and (l >= r);
		if found then begin
			writeln(k);
			for j := 0 to k-2 do write(p[j], ' ');
			if k > 0 then write(p[k-1]);
			writeln;
		end;

	end;
end;

begin
	readln(notc);
	for tci := 1 to notc do begin

		p := TList<int32>.Create;
		x := TList<int32>.Create;

		readln(n);
		readln(s);

		found := false;
		dfs(1);
		if not found then
			writeln(-1);

		p.Free;
		x.Free;

	end;
end.

```
