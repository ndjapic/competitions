class Solution:
    def removeAlmostEqualCharacters(self, word: str) -> int:

        def alm(a, b: int) -> Boolean:
            return abs(ord(word[a]) - ord(word[b])) < 2

        answer = 0

        i, n = 0, len(word)
        while i < n-1:
            if alm(word[i-1], word[i]):
                answer += 1
                i += 2
            else:
                i += 1

        if i < n-1 and alm(word[i-1], word[i]):
                answer += 1
        
        return answer
