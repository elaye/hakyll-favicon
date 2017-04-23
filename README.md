# hakyll-favicon

This library allows you to easily add favicons to your hakyll website.

<!-- It works like this: you provide an SVG favicon file and the library will generate the different favicon resolutions and the corresponding html. -->
<!-- It is recommended for your main favicon to be an SVG. -->
<!-- Alternatively you can use a PNG with at least the highest resolution required by the favicons to be generated. -->
<!-- If your main favicon image is too small, it will be scaled up and the result might be of bad quality. -->
<!-- The image you provide also needs to have an aspect ratio of 1:1 (same width and height). -->
It works like this: you provide an SVG with an aspect ratio of 1:1 (same width and height) and the library will generate the different favicon resolutions and the corresponding html.

To make this work you need to do 3 things.
First, add a `faviconsRules` that points to your main favicon in your `Site.hs` file like this:

```
main = hakyll $ do
  faviconsRules "images/favicon.svg" -- <- path to your favicon
  ...
```

Second, add a `favicons` field in your template `head`:

```
<head>
  ...
  $favicons$
</head>
```

Finally add the favicons context to your template context:

```
main = hakyll $ do
  ...
  match "index.html" $ do
    ...
    let ctx = ... `mappend`
              ... `mappend`
              faviconsField `mappend` -- add this
              defaultContext
    ...
```

