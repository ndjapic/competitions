program fastio;
{$MODE DELPHI}{$INLINE ON}
uses
	Generics.Defaults, Generics.Collections, sysutils, classes, math;
const
	nn = 200 * 1000;
type
	TIntComparer = class(TComparer<int32>)
		function Compare(constref L, R: int32): Integer; override;
	end;
var
	notc, tci: int32;
	n, i: int32;
	a: TList<int32>;
	sl: TStringList;
	ios: string;
	Comparer: TIntComparer;
	ans: array [0 .. nn] of int32;

function TIntComparer.Compare(constref L, R: int32): Integer;
begin
	Result := L - R;
end;

begin
	randomize;
	sl := TStringList.Create;
	sl.Delimiter := ' ';
	a := TList<int32>.Create;
	Comparer := TIntComparer.Create;
	Comparer._AddRef;

	readln(notc);
	for tci := 1 to notc do begin

		readln(n);

		readln(ios);
		sl.DelimitedText := ios;

		a.Clear;
		for i := 0 to n-1 do begin
			a.Add(StrToInt(sl[i]));
			a.Exchange(i, random(i+1));
		end;
		a.Sort(Comparer);

		sl.Clear;
		for i := 0 to a.Count -1 do begin
			ans[i] := sqr(a[i]);
			sl.Add(IntToStr(ans[i]));
		end;

		writeln(sl.DelimitedText);
		flush(StdErr); flush(output); // DO NOT REMOVE

	end;

	FreeAndNil(sl);
	FreeAndNil(a);
	Comparer._Release;
	{Comparer.Free;}
end.
(*
	1
	5
	2  5  3  4  1  
	1 4 9 16 25


	------------------
	(program exited with code: 0)
	Press return to continue
* )
