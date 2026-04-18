# Задатак: A_Easy_As_ABC.pas

```pascal
program A_Easy_As_ABC;
uses
    math;
var
    i, j, ans: int8;
    ch: char;
    a: array [1 .. 3, 1 .. 3] of int8;
    seen: array [1 .. 3, 1 .. 3] of boolean;

procedure dfs(i, j, n, wrd: int8);
begin
    if (0 < i) and (i <= 3) and (0 < j) and (j <= 3) and not seen[i, j] then begin

        wrd := wrd * 3 + a[i, j];
        inc(n);
        seen[i, j] := true;

        if n = 3 then
            ans := min(ans, wrd)
        else begin

            dfs(i-1, j, n, wrd);
            dfs(i+1, j, n, wrd);
            dfs(i, j-1, n, wrd);
            dfs(i, j+1, n, wrd);

            dfs(i-1, j-1, n, wrd);
            dfs(i-1, j+1, n, wrd);
            dfs(i+1, j-1, n, wrd);
            dfs(i+1, j+1, n, wrd);

        end;

        seen[i, j] := false;

    end;
end;

begin

    for i := 1 to 3 do begin
        for j := 1 to 3 do begin
            read(ch);
            a[i, j] := ord(ch) - ord('A');
            seen[i, j] := false;
        end;
        readln;
    end;

    ans := 26;
    for i := 1 to 3 do
        for j := 1 to 3 do
            dfs(i, j, 0, 0);

    ch := chr(ord('A') + ans div 9);
    write(ch);
    ans := ans mod 9;

    ch := chr(ord('A') + ans div 3);
    write(ch);

    ch := chr(ord('A') + ans mod 3);
    writeln(ch);

end.

```
