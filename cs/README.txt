convention
==========
    a very opinionated coupon site with sensible defaults

    [entity|relation-entity]:[entity_id|relation_id] => value
    where relation is the relationship of entity to other entity

search/discovery:
=================
    merchants are searchable by 'name' and case insensitive

    navigation is implemented with a very simple custom query string
    where each key:value pair is a filter and some of them may be
    AND or OR'ed, the result sets are already precomputed in redis
        merchant: undefined|best-buy|...
        category: undefined|electronics|...
        type: undefined|coupon|exclusive|...
        sort: recent|rank, default: recent, paginated, always descending

    all links sorted by ranked/recent
        GET /v1/links?&sort=rank|recent
        links = [link_id] (list)
        ranked-links = [link_id] (sorted_set, background process)

    all links of merchant X sorted by recent
        GET /v1/links?merchant=merchant_id&sort=recent
        merchant-links:merchant_id = [link_id] (list)
        (TODO): ranked-merchant-links

    all links of category X sorted by recent
        GET /v1/links?category=category_id&sort=recent
        category-links:category_id = [link_id] (list)
        (TODO): ranked-category-links

    all links of type X sorted by recent
        type-links:type_id = [link_id] (list)
        (TODO): ranked-type-links

    top merchants / recently added merchants
        GET /v1/merchants?sort=rank
        merchants = [merchant_id] (list)
        ranked-merchants = [merchant_id] (sorted_set)

    category merchant
        GET /v1/merchants?category=category_id
        category-merchants:category_id = [merchant_id] (list)

keys
====
merchant
    merchant:uuid => fields (hash)
    merchant-permalink:permalink => uuid (key), inverse index by name

category
    category-merchants:category_id => [merchant_id] (list)
    category-links:category_id => [link_id] (list)

link
    link:uuid => fields (hash)
    type-links:type_id => [link_id] (list)
    category-links:category_id => [link_id] (list)
    merchant-links:merchant_id => [link_id] (list)

stats (hash)
    merchants => merchant count (key)
    links => link count (key)
    category-merchants:category_id => count of merchants in this category (key)
    category-links:categor_id => count of links in this category (key)
