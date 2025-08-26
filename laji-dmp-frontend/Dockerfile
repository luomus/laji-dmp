FROM node:22-alpine3.22 AS builder

WORKDIR /app/dmp-frontend
COPY package*.json ./
RUN npm install
COPY . .
RUN npm run build

FROM nginx:alpine3.22-slim AS front-end

COPY ./nginx.conf /etc/nginx/conf.d/default.conf
COPY --from=builder /app/dmp-frontend/dist /usr/share/nginx/html/

CMD ["nginx", "-g", "daemon off;"]