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
	enu : TList<int32>.TEnumerator;
	a: TList<int32>;
	sl: TStringList;
	ios, s: string;
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

		readln(n); // Note: Local variable "n" is assigned but never used
		readln(ios);
		sl.DelimitedText := ios;

		a.Clear;
		for s in sl do begin
			a.Add(StrToInt(s));
			a.Exchange(a.Count - 1, random(a.Count));
		end;
		a.Sort(Comparer);

		sl.Clear;
		enu := a.GetEnumerator;
		for i := 0 to a.Count -1 do
			if enu.MoveNext then begin
				ans[i] := sqr(enu.GetCurrent);
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
*)
