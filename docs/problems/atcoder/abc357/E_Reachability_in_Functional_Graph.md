# Problem: E_Reachability_in_Functional_Graph.pas

```pascal
program E_Reachability_in_Functional_Graph;
uses
    math;
const
    nn = 200 * 1000;
var
    n, i: int32;
    s: int64;
    a, r: array [1 .. nn] of int32;
    seen: array [1 .. nn] of boolean;

procedure dfs1(i: int32);
var
    j: int32;
begin
    if not seen[i] then begin
        seen[i] := true;
        j := a[i];
        dfs1(j);
        r[i] := r[j] + 1;
        seen[i] := false;
    end;
end;

procedure dfs2(i: int32);
var
    j: int32;
begin
    if not seen[i] then begin
        seen[i] := true;
        j := a[i];
        dfs2(j);
        r[i] := max(r[i], r[j]);
    end;
end;

begin
    readln(n);

    for i := 1 to n do begin
        read(a[i]);
        seen[i] := false;
        r[i] := 0;
    end;
    readln;

    for i := 1 to n do begin
        dfs1(i);
        dfs2(i);
    end;

    s := 0;
    for i := 1 to n do inc(s, r[i]);
    writeln(s);
end.

```
