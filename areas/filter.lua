-- filter.lua
-- Convert theorem-like environments to Org special blocks

local envs = {
  theorem = true,
  lemma = true,
  corollary = true,
  proposition = true,
  definition = true,
  remark = true,
  proof = true,
}

local function has_env_class(classes)
  for _, c in ipairs(classes) do
    if envs[c] then return c end
  end
  return nil
end

-- Best case: theorem env parsed as a Div with a class (e.g. {theorem})
function Div(el)
  local env = has_env_class(el.classes)
  if not env then return nil end

  local out = pandoc.Blocks({})
  out:insert(pandoc.RawBlock("org", "#+begin_" .. env))
  for _, b in ipairs(el.content) do
    out:insert(b)
  end
  out:insert(pandoc.RawBlock("org", "#+end_" .. env))
  return out
end

-- Fallback: if begin/end come through as raw LaTeX blocks
function RawBlock(el)
  if el.format ~= "latex" then return nil end

  local b = el.text:match("^\\begin%{([%w%-]+)%}%s*$")
  if b and envs[b] then
    return pandoc.RawBlock("org", "#+begin_" .. b)
  end

  local e = el.text:match("^\\end%{([%w%-]+)%}%s*$")
  if e and envs[e] then
    return pandoc.RawBlock("org", "#+end_" .. e)
  end
end
