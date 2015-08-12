-record(state, {mod       :: module(),
                method    :: binary(),
                path      :: binary()}).

-define(CATEGORIES, ["accessories",
                     "automotive",
                     "baby-and-kids",
                     "books",
                     "clothing",
                     "electronics",
                     "entertainment",
                     "food-and-drinks",
                     "flowers",
                     "furnitures",
                     "gifts",
                     "health-and-beauty",
                     "home-and-garden",
                     "household-essentials",
                     "jewelry-and-watches",
                     "musical-instruments",
                     "office-supplies",
                     "party-supplies",
                     "photography",
                     "services",
                     "shoes",
                     "men's-fashion",
                     "women's-fashion",
                     "sports-outdoors",
                     "toys",
                     "travel"]).

-define(TYPES, ["buy-one-get-one-free",
                "free-shipping",
                "exclusive",
                "printables"
                "coupon",
                "others"]). % promotion, marketing links?

-define(SORTS, ["recent", "score"]). % always descending
