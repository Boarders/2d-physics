default: ps
	yarn parcel web/index.html

deps:
	yarn add --dev parcel-bundler

ps:
	spago bundle-app --main ThreeBody --to 'web/Main.js'
