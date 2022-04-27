# Pre-Requisites

1. Postgress service.
2. Imagga Account's Auth Key.

# Build

`cabal build`

# Run

cabal run imageAnalyzer -- \
   --dbname $DbName \
   --user $DBUserId \
   --password $DBPASSWORD \
   --authcode $IMAGGAAPIAUTHKEY

# Test

1. Start the program.
2. Upload images using the following curl command.
`curl -v -H 'Content-Type: application/json' -d '{"url":"$PATH_TO_IMAGE", "source":"file", "is_det":true, "label":"gadget"}' -X POST "localhost:8081/upload"`
3. Get all the images.
`curl -v "http://localhost:8081/images"`
4. Get all the images having `notebook`.
`curl -v "http://localhost:8081/images_detect?objects=notebook"`
5. Get Image with required ID
`curl -v "http://localhost:8081/images/{imageId}"`
