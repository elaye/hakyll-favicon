# hakyll-favicon

This library allows you to easily add favicons to your hakyll website.
You provide one SVG image and the library will convert it to different resolutions and generate the corresponding html.

## Dependencies

This library depends on `ImageMagick` to convert the images.

## Usage

First, add a `faviconsRules` that points to your main favicon in your `Site.hs` file like this:

```
main = hakyll $ do
  faviconsRules "images/favicon.svg" -- path to your favicon
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

The [example](example/) directory provides a minimal working example of this.

## Example

First, build the example:

    stack build

Then build the example page:

    stack exec example build
    
Or you can start a local server to serve the generated page:

    stack exec example watch

## Generated favicons

The following favicons are generated:

target       | format | sizes  | description
-------------|--------|--------|------------
* | .ico | 32, 64 | basic favicon
* | .png | 32 | basic favicon
iOS | .png | 144 | third-generation iPad with high-resolution Retina display
iOS | .png | 114 | iPhone with high-resolution Retina display
iOS | .png | 72 | first- and second-generation iPad
iOS, Android | .png | 57 | non-Retina iPhone, iPod Touch, and Android 2.1+ devices
